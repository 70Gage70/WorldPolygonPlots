(* ::Package:: *)

(* ::Section:: *)
(*WorldPolygons*)


BeginPackage["WorldPolygons`"]

WorldPolygon::usage = "WorldPolygon[] returns a Polygon representing Earth landmass suitable for use in Graphics."
	WorldOpacity::usage = "The opacity of the landmass."
	WorldColor::usage = "The color of the landmass."
	
WorldGeoPolygon::usage = "WorldPolygon[] returns a Polygon representing Earth landmass suitable for use in GeoGraphics."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


(*None*)


(* ::Subsection:: *)
(*WorldPolygon*)


WorldPolygonOpts={WorldOpacity->1, WorldColor->RGBColor[0.65, 0.65, 0.65]};
Options[WorldPolygon]=WorldPolygonOpts;

WorldPolygon[opts:OptionsPattern[]]:=
	With[{WorldOpacity=OptionValue[WorldOpacity], WorldColor=OptionValue[WorldColor]},
		{Opacity[WorldOpacity], FaceForm[WorldColor], EdgeForm[None], CountryData["World", "Polygon"]}
	]


(* ::Subsection:: *)
(*WorldGeoPolygon*)


Options[WorldGeoPolygon]=Options[WorldPolygon];

WorldGeoPolygon[opts:OptionsPattern[]]:=
	With[{WorldOpacity=OptionValue[WorldOpacity], WorldColor=OptionValue[WorldColor]},
		{GeoStyling[Opacity[WorldOpacity]], FaceForm[WorldColor], EdgeForm[None], CountryData["World", "Polygon"]}
	]


End[]


EndPackage[]
