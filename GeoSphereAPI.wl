(* ::Package:: *)

BeginPackage["GeoSphereAPI`"]

GeoSphereAPIEndpoints::usage = 
	"GeoSphereAPIEndpoints returns an Association of all available endpoints in the Geosphere API.
	It includes information on the type and mode of the data that can be retrieved."
	
GeoSphereAPIResources::usage =
	"Get a list of datasets available via the GeoSphere API."
	
GeoSphereAPIMetadata::usage = 
	"Get the metadata for a given resource."
	
GeoSphereAPIResourceParameters::usage = 
	"Get the parameters of a given resource."

GeoSphereAPIRequest::usage = 
	"Request data from the GeoSphere API"

Begin["`Private`"]

(* set some necessary global variables *)
$GeoSphereAPIBasePath = "https://dataset.api.hub.zamg.ac.at";
$GeoSphereAPIVersion = "v1";
$GeoSphereAPIResponseFormats = List["geojson", "csv"];
$GeoSphereAPIDefaultResponseFormat = $GeoSphereAPIResponseFormats[[1]];

(*Get an Association of API endpoints*)
GeoSphereAPIEndpoints[apiBasePath_:$GeoSphereAPIBasePath, apiVersion_:$GeoSphereAPIVersion]:=
	Module[{basePath = apiBasePath, 
		version = apiVersion, 
		response, 
		shortKeys},
		response =Association[Import[URL[StringRiffle[List[basePath,version, "datasets"],"/" ]], "JSON"]];
		shortKeys =Map[Part[StringSplit[#,"/"],3]&, Keys[response]];
		MapApply[Association,
			KeyMap[Replace[MapApply[Rule,Transpose[List[Keys[response], shortKeys]]]]][response]]
	]
	
(* List the available API resources (datasets) *)
GeoSphereAPIResources[]:=Keys[GeoSphereAPIEndpoints[]]

(*  Extract the URL of a given resource *)
getResourceURL[resource_]:=Module[{res = resource, endpoints}, 
	endpoints = GeoSphereAPIEndpoints[];
	If[MemberQ[Keys[endpoints], res], endpoints[res]["url"], List[]]]


(*  Get the metadata of a given resource *)
GeoSphereAPIMetadata[resource_] := 
	Module[{thisResource = resource,
			response},
			response = Association[Import[URL[StringJoin[getResourceURL[thisResource], "/metadata"]]]]]

(* Retrieve the parameters of a given resource *)			
GeoSphereAPIResourceParameters[resource_]:=
	MapApply[Association, GeoSphereAPIMetadata[resource]["parameters"]]	

(* Construct the main query *)
constructQueryString[parameters_List, 
	startDate_DateObject, 
	endDate_DateObject, 
	position_GeoPosition, 
	responseFormat_String]:=
	Module[{pars = parameters,
			start = DateString[startDate, "ISODate"],
			end = DateString[endDate, "ISODate"],
			latLong = position["LatitudeLongitude"],
			format = StringJoin["output_format=",responseFormat],
			parString,
			dateString,
			latLongString,
			queryString,
			sep = "&"},
		parString = StringRiffle[Map[StringJoin["parameters=", #]&, pars], sep];
		dateString = StringRiffle[List[StringJoin["start=", start], StringJoin["end=", end]], sep];
		latLongString = StringJoin["lat_lon=", StringRiffle[latLong, "%2C"]];
		queryString = StringRiffle[List[parString, dateString,latLongString, format], sep]
	]
	
	
(* Main function to request data from the API *)
Protect[GeoSphereAPIResponseFormat];	
Options[GeoSphereAPIRequest]={GeoSphereAPIResponseFormat :> $GeoSphereAPIDefaultResponseFormat};			
GeoSphereAPIRequest[resource_, 
	position_GeoPosition, 
	startDate_DateObject, 
	endDate_DateObject, 
	opts : OptionsPattern[]] := 
		Module[{response},
		response = 
			Import[URL[StringJoin[
						getResourceURL[resource], "?",
						constructQueryString[GeoSphereAPIResourceParameters[resource][[All, "name"]],
							startDate, endDate, position, OptionValue["GeoSphereAPIResponseFormat"]]]]]];
			

End[]
EndPackage[]



