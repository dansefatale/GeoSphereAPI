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

GeoSphereGetWeatherMap::usage =
	"Request a weathermap from the GeoSphere website."

Begin["`Private`"]

(* set some necessary global variables *)
$GeoSphereAPIBasePath = "https://dataset.api.hub.geosphere.at";
$GeoSphereAPIVersion = "v1";
$GeoSphereAPIResponseFormats = List["GeoJSON", "CSV"];
$GeoSphereAPIDefaultResponseFormat = $GeoSphereAPIResponseFormats[[1]];

(*Get an Association of API endpoints*)
GeoSphereAPIEndpoints[apiBasePath_:$GeoSphereAPIBasePath, apiVersion_:$GeoSphereAPIVersion]:=
	Module[{basePath = apiBasePath, 
		version = apiVersion, 
		response, 
		shortKeys},
		response =Association[
			Import[URL[StringRiffle[List[basePath,version, "datasets"],"/" ]], "JSON"]];
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
GeoSphereAPIRequest[resource_, 
	position_GeoPosition, 
	startDate_DateObject, 
	endDate_DateObject] := 
		Module[{formatValue = $GeoSphereAPIDefaultResponseFormat,
		rawResponse,
		parameters,
		timestamps,
		response},
		
		(* get the raw API response in JSON format and turn it to an Association on the highest level *)
		rawResponse = 
			Association[Import[URL[StringJoin[
						getResourceURL[resource], "?",
						constructQueryString[GeoSphereAPIResourceParameters[resource][[All, "name"]],
							startDate, endDate, position, ToLowerCase[formatValue]]]],
							{formatValue, "Data"}]];
							
		(* Use pattern matching to pick out the actual parameters and 
		turn them into an associtation of associations *)
		parameters = 
			Map[Association, 
				Apply[Association,
					Flatten[Cases[rawResponse["Features"],
						KeyValuePattern[key_->{"Name"->_String, "unit"->_String, "data"->_List}], Infinity]]]];
			
		(* Convert time stamps to DateObjects *)							
		timestamps = List[Map[DateObject, rawResponse["timestamps"]]];	
			
		(* Finally, we want to replace the "data" entry with a timeseries of timestamps and data and return the result *)
		response = MapAll[
			Evaluate,
			Replace[
				parameters,
					<|name_ -> n_, unit_ -> u_, data_ -> d_|> :> <|name -> n, unit -> u,
						data -> TimeSeries[d, timestamps]|>,{1}]]
						
		];
		
	
(* Function to request a weather map from the GeoSphere website *)
GeoSphereGetWeatherMap[date_DateObject]:= 
	Module[{requestHour,
		requestDate,
		basePath = "https://www.zamg.ac.at/fix/wetter/bodenkarte",
		reqString,
		sep = "/",
		imgNamePrefix = "BK_BodAna_Sat_",
		imgDate,
		imgType = ".png"}, 
		
		(* From the given Date Object floor to the next possible request hour:
		00:00, 06:00, 12:00, 18:00. If we do not find any Hour in the date object, set
		it to midnight *)
		requestHour = Quiet[Check[List[Floor[N[DateValue[date, "Hour"]/6]]*6, 0, 0],
			List[0,0,0]]]; 
			
		requestDate = StringSplit[DateString[date, "ISODate"], "-"];
		
		imgDate = StringJoin[
			DateString[date, "YearShort"],
			DateString[date,"Month"],
			DateString[date, "Day"],
			Map[ToString,
				MapAt[If[# < 10, StringJoin["0", ToString[#]], ToString[#]]&,
					requestHour, 1]]];
		
		reqString = 
			StringRiffle[Flatten[List[
				basePath,
				requestDate,
				StringJoin[imgNamePrefix,
					imgDate,
					imgType]]], sep];
		
		Import[reqString]
	]		

End[]
EndPackage[]





