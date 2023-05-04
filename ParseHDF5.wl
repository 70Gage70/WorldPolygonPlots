(* ::Package:: *)

(* ::Section:: *)
(*ParseHDF5*)


BeginPackage["ParseHDF5`"]

ParseHDF5Polygons::usage = "ParseHDF5Polygons[file] creates a list of polygons and disconnected polygons from the vertices in the input file."

ParseABInds::usage = "ParseABInds[file] creates a list of TPT indices of A, B and the avoided region from the input file."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


(*None*)


(* ::Subsection:: *)
(*ParseHDF5Polygons*)


ParseHDF5PolygonsOpts={PolysDirectory->"/ulam/polys", NPolysDisDirectory->"/ulam/n_polys_dis", PolysDisDirectory->"/ulam/polys_dis", PolyType->Polygon};
Options[ParseHDF5Polygons]=ParseHDF5PolygonsOpts;

ParseHDF5Polygons[file_,opts:OptionsPattern[]]:=
	With[{PolysDirectory=OptionValue[PolysDirectory], NPolysDisDirectory=OptionValue[NPolysDisDirectory], PolysDisDirectory=OptionValue[PolysDisDirectory], PolyType=OptionValue[PolyType]},
	Module[{polysin, vcells, polysdisin, vcellsdis},
		polysin=Transpose[Import[file, PolysDirectory]];
		vcells=Table[PolyType[Select[polysin, #[[3]] == i &][[All, 1;;2]]], {i, 1, Max[polysin[[All, 3]]]}];
		vcellsdis=If[Import[file,NPolysDisDirectory]==0,
			{},
			polysdisin=Transpose[Import[file,PolysDisDirectory]];
			Table[PolyType[Select[polysdisin, #[[3]] == i &][[All, 1;;2]]], {i, 1, Max[polysdisin[[All, 3]]]}]
			];
		{vcells, vcellsdis}
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


End[]


EndPackage[]
