(* ::Package:: *)

(* ::Section:: *)
(*PolygonColors*)


BeginPackage["PolygonColors`"]

ParseHDF5Polygons::usage = "ParseHDF5Polygons[file, opts] creates a list of polygons and disconnected polygons from the vertices in the input file."

ParseABInds::usage = "ParseABInds[file, opts] creates a list of TPT indices of A, B and the avoided region from the input file."

PolygonColors::usage = "PolygonColors[file, opts] returns a list of {polys, polysdis}."
	AColor::usage = "The color of A."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`
<<EurekaColors`
<<WLHelpers`


(* ::Subsection:: *)
(*ParseHDF5Polygons*)


ParseHDF5PolygonsOpts={PolysDirectory->"/ulam/polys", NPolysDisDirectory->"/ulam/n_polys_dis", PolysDisDirectory->"/ulam/polys_dis", PolyOrGeoPoly->"Polygon"};
Options[ParseHDF5Polygons]=ParseHDF5PolygonsOpts;

ParseHDF5Polygons[file_,opts:OptionsPattern[]]:=
	With[{PolysDirectory=OptionValue[PolysDirectory], NPolysDisDirectory=OptionValue[NPolysDisDirectory], PolysDisDirectory=OptionValue[PolysDisDirectory], PolyOrGeoPoly=OptionValue[PolyOrGeoPoly]},
	Module[{polysin, vcells, polysdisin, vcellsdis},
		polysin=Transpose[Import[file, PolysDirectory]];
		vcells=Table[Select[polysin, #[[3]] == i &][[All, 1;;2]], {i, 1, Max[polysin[[All, 3]]]}];
		vcellsdis=If[Import[file,NPolysDisDirectory]==0,
			{},
			polysdisin=Transpose[Import[file,PolysDisDirectory]];
			Table[Select[polysdisin, #[[3]] == i &][[All, 1;;2]], {i, 1, Max[polysdisin[[All, 3]]]}]
			];
		Which[
				PolyOrGeoPoly=="Polygon", {Map[Polygon,vcells], Map[Polygon,vcellsdis]},
				PolyOrGeoPoly=="GeoPolygon", {Map[GeoPolygon,Reverse[vcells,3]], Map[GeoPolygon,Reverse[vcellsdis,3]]}
			]
		]
	]


(* ::Subsection:: *)
(*ParseABInds*)


ParseABIndsOpts={IndsADirectory->"/tpt_stat/indices/A", IndsBDirectory->"/tpt_stat/indices/B"};
Options[ParseABInds]=ParseABIndsOpts;

ParseABInds[file_,opts:OptionsPattern[]]:=
	With[{IndsADirectory=OptionValue[IndsADirectory], IndsBDirectory=OptionValue[IndsBDirectory]},
	Module[
	{
	Ainds = Import[file, IndsADirectory], 
	Binds = Import[file, IndsBDirectory], 
	indsAvoid
	}, 
		indsAvoid = Intersection[Ainds, Binds];
		{Ainds, Binds, indsAvoid}
		]
	]


(* ::Subsection:: *)
(*PolygonColors*)


PolygonColorsOpts={
	ScalarDirectory->"tpt_stat/statistics/normalized_reactive_density",
	PolygonOpacity->0.8, 
	PolygonColorScaled->True, 
	PolygonColorFunction->EurekaColorSmooth,
	PolygonColorTransform->Function[#^(1/4)],
	AColor->Hue[0.83,0.68,1.0], 
	BColor->Red, 
	DisconColor->Black, 
	AvoidColor->LightGray};
	
Options[PolygonColors]=Join[
					PolygonColorsOpts,
					Options[ParseHDF5Polygons],
					Options[ParseABInds]
					];

PolygonColors[file_, opts:OptionsPattern[]]:=
	With[{
	ScalarDirectory = OptionValue[ScalarDirectory], 
	PolygonOpacity = OptionValue[PolygonOpacity],
	PolygonColorScaled = OptionValue[PolygonColorScaled],
	PolygonColorFunction = OptionValue[PolygonColorFunction],
	PolygonColorTransform = OptionValue[PolygonColorTransform],
	AColor = OptionValue[AColor],
	BColor = OptionValue[BColor],
	DisconColor = OptionValue[DisconColor],
	AvoidColor = OptionValue[AvoidColor],
	PolyOrGeoPoly=OptionValue[PolyOrGeoPoly]}, 
	Module[{polys, polysDis, GeoPolyOpacity, indsA, indsB, indsAvoid, scalar, maxScalar, polycolor, polycolorDis},
		{polys, polysDis} = ParseHDF5Polygons[file, DelegateOptions[opts, PolygonColors]];
		{indsA, indsB, indsAvoid}=ParseABInds[file, DelegateOptions[opts, PolygonColors]];
		scalar=Import[file,ScalarDirectory];
		If[Length[scalar]!=Length[polys],Print["Scalar and polys have different dimensions."]; Abort[]];
		maxScalar=If[PolygonColorScaled,Max[scalar],1];
		GeoPolyOpacity=Which[
			PolyOrGeoPoly=="Polygon", Function[Opacity[#]], 
			PolyOrGeoPoly=="GeoPolygon", Function[GeoStyling[Opacity[#]]]
			];
		polycolor = 
			Table[
				Which[
					MemberQ[indsAvoid, i],{GeoPolyOpacity[1],FaceForm[AvoidColor], polys[[i]]},
					MemberQ[indsA, i],{GeoPolyOpacity[1], FaceForm[AColor], EdgeForm[Directive[Thin, LightGray]], polys[[i]]},
					MemberQ[indsB, i],{GeoPolyOpacity[1], FaceForm[BColor], EdgeForm[Directive[Thin, LightGray]], polys[[i]]},
					True, {GeoPolyOpacity[PolygonOpacity], FaceForm[PolygonColorFunction[PolygonColorTransform[scalar[[i]]/maxScalar]]], EdgeForm[Directive[Thin, LightGray]], polys[[i]]}
					],
			{i,1,Length[polys]}
			];
		polycolorDis=
			Table[
			{FaceForm[DisconColor], polysDis[[i]]},
			{i,1,Length[polysDis]}
			];
		{polycolor, polycolorDis}
		]	
	]	


End[]


EndPackage[]
