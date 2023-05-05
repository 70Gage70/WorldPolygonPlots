(* ::Package:: *)

(* ::Section:: *)
(*PolygonColorsLegends*)


BeginPackage["PolygonColorsLegends`"]

ParseHDF5Polygons::usage = "ParseHDF5Polygons[file, opts] creates a list of polygons and disconnected polygons from the vertices in the input file."
	PolysDirectory::usage = "The directory in the HDF5 file for the polygon vertices."
	NPolysDisDirectory::usage = "The directory in the HDF5 file for the number of disconnected polygons."
	PolysDisDirectory::usage = "The directory in the HDF5 file for the disconnected polygon vertices."
	PolyOrGeoPoly::usage = "Whether to parse the vertices as Polygon or GeoPolygon."

ParseABInds::usage = "ParseABInds[file, opts] creates a list of TPT indices of A, B and the avoided region from the input file."
	IndsADirectory::usage = "The directory in the HDF5 file for the A indices."
	IndsBDirectory::usage = "The directory in the HDF5 file for the B indices."
	
PolygonColors::usage = "PolygonColors[file, opts] returns a list of {polys, polysdis}."
	ScalarDirectory::usage = "The directory in the HDF5 file for the scalar quantity used to color the polygons."
	ScalarParts::usage = "Extract[scalar, ScalarParts] is applied to the scalar."
	PolygonOpacity::usage = "The opacity of the polygons the polygons."
	PolygonColorScaled::usage = "Whether to scale the value of the scalar by its maximum."
	PolygonColorFunction::usage = "A map from real numbers to colors."
	PolygonColorTransform::usage = "A function that the values of the scalar will be transformed by."
	PolygonColorEdgeForm::usage = "The edge form of polygons other than true A and B."
	AColor::usage = "The color of the A states."
	AEdgeForm::usage = "The EdgeForm of A states."
	BColor::usage = "The color of the B states."
	BEdgeForm::usage = "The EdgeForm of B states."
	DisconColor::usage = "The color of the disconnected states."
	AvoidColor::usage = "The color of the avoided states."


ABLegend::usage = "ABLegend[opts] creates a legend suitable for TPT plots."
	ABPlaced::usage = "Control the placement location of ABLegend."
	ABLegendMag::usage = "Control the size of the text labels."
	ABLegendMarkerSize::usage = "Control the size of the swatch boxes."
			
PolygonBarLegend::usage = "Add a bar legend for the data."

PolygonSwatchLegend::usage = "Add a swatch legend for the data."
	PolygonDataLegendSwatchOffset::usage = "The offset between text and markers."
	PolygonDataLegendSwatchSpacings::usage = "The vertical spacing between the markers (unstable for negative values.)"
	
(*Options common to all data legends.*)	
	PolygonDataLegendLabel::usage = "A TeX string for swatch legends or list of three TeX strings for bar legends."
	PolygonDataLegendPlaced::usage = "The location of the legend."
	PolygonDataLegendLabelMag::usage = "The size of the text of the legend label."
	PolygonDataLegendMarkerSize::usage = "The size of the marker(s)."
	PolygonDataLegendTickMag::usage = "The size of the legend ticks/labels."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`
<<EurekaColors`
<<WLHelpers`


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*PolygonColors*)


PolygonColorsOpts={
	ScalarDirectory->"tpt_stat/statistics/normalized_reactive_density",
	ScalarParts->{All},
	PolygonOpacity->0.8, 
	PolygonColorScaled->True, 
	PolygonColorFunction->EurekaColorSmooth,
	PolygonColorTransform->Function[#^(1/4)],
	PolygonColorEdgeForm->Directive[Thin, LightGray],
	AColor->Hue[0.83,0.68,1.0],
	AEdgeForm->Directive[Thin, LightGray], 
	BColor->Red, 
	BEdgeForm->Directive[Thin, LightGray],
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
	ScalarParts = OptionValue[ScalarParts], 
	PolygonOpacity = OptionValue[PolygonOpacity],
	PolygonColorScaled = OptionValue[PolygonColorScaled],
	PolygonColorFunction = OptionValue[PolygonColorFunction],
	PolygonColorTransform = OptionValue[PolygonColorTransform],
	PolygonColorEdgeForm=OptionValue[PolygonColorEdgeForm],
	AColor = OptionValue[AColor],
	AEdgeForm = OptionValue[AEdgeForm],
	BColor = OptionValue[BColor],
	BEdgeForm = OptionValue[BEdgeForm],
	DisconColor = OptionValue[DisconColor],
	AvoidColor = OptionValue[AvoidColor],
	PolyOrGeoPoly=OptionValue[PolyOrGeoPoly]}, 
	Module[{polys, polysDis, GeoPolyOpacity, indsA, indsB, indsAvoid, trueAB, scalar, maxScalar, polycolor, polycolorDis},
		{polys, polysDis} = ParseHDF5Polygons[file, DelegateOptions[opts, PolygonColors]];
		{indsA, indsB, indsAvoid}=ParseABInds[file, DelegateOptions[opts, PolygonColors]];
		scalar=Extract[Import[file,ScalarDirectory],ScalarParts];
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
					MemberQ[indsA, i],{GeoPolyOpacity[1], FaceForm[AColor], EdgeForm[AEdgeForm], polys[[i]]},
					MemberQ[indsB, i],{GeoPolyOpacity[1], FaceForm[BColor], EdgeForm[BEdgeForm], polys[[i]]},
					True, {GeoPolyOpacity[PolygonOpacity], FaceForm[PolygonColorFunction[PolygonColorTransform[scalar[[i]]/maxScalar]]], EdgeForm[PolygonColorEdgeForm], polys[[i]]}
					],
			{i,1,Length[polys]}
			];
		trueAB=Complement[Union[indsA,indsB],indsAvoid];
		polycolor=Join[Delete[polycolor, List /@ trueAB], Part[polycolor, trueAB]]; (*Put true A/B at the end so they take priority in graphics*)
		polycolorDis=
			Table[
			{FaceForm[DisconColor], polysDis[[i]]},
			{i,1,Length[polysDis]}
			];
		{polycolor, polycolorDis}
		]	
	]	


(* ::Subsection::Closed:: *)
(*ABLegend*)


ABLegendOpts={ABPlaced->{0.077,0.18}, ABLegendMag->1.8, ABLegendMarkerSize->12};
Options[ABLegend]=Join[
					ABLegendOpts,
					FilterRules[Options[PolygonColors],{AColor,BColor,DisconColor,AvoidColor}]];

ABLegend[opts:OptionsPattern[]]:=
	With[{AColor = OptionValue[AColor], BColor = OptionValue[BColor], DisconColor = OptionValue[DisconColor], AvoidColor = OptionValue[AvoidColor], ABPlaced = OptionValue[ABPlaced], ABLegendMag = OptionValue[ABLegendMag], ABLegendMarkerSize = OptionValue[ABLegendMarkerSize]}, 
	Module[{colors,labels,valid,legs},
		colors={AColor,BColor,DisconColor,AvoidColor};
		labels={MaTeX["\\mathbb{A}", Magnification->ABLegendMag],
				MaTeX["\\mathbb{B}", Magnification->ABLegendMag],
				MaTeX["\\text{Discon.}", Magnification->ABLegendMag],
				MaTeX["\\text{Avoid}", Magnification->ABLegendMag]};
		valid=Flatten[Position[colors, _?(ColorQ[#]&)]];
		colors=colors[[valid]];
		labels=labels[[valid]];
		Placed[
			SwatchLegend[
				colors,
				labels,
				LegendMarkerSize->ABLegendMarkerSize,
				LegendMarkers->Table[
								Graphics[{Opacity[1],EdgeForm[Directive[Thin,Black]],Rectangle[]}],
							{i,1,Length[colors]}]
				],
			ABPlaced
			]
		]
	]


(* ::Subsection:: *)
(*PolygonDataLegend*)


PolygonDataLegendOpts={
	PolygonDataLegendLabel->{"\\left(", "\\mu^{\\mathbb{A} \\mathbb{B}}", "\\right)^{1/4}"}, 
	PolygonDataLegendPlaced->{0.9, 0.8}, 
	PolygonDataLegendLabelMag->1.2, 
	PolygonDataLegendMarkerSize->{10,100}, 
	PolygonDataLegendTickMag->1};


(* ::Subsection:: *)
(*PolygonBarLegend*)


Options[PolygonBarLegend]=Join[
						PolygonDataLegendOpts,
						FilterRules[Options[PolygonColors],{ScalarDirectory,ScalarParts,PolygonColorFunction,PolygonColorScaled}]
						];

PolygonBarLegend[file_,opts:OptionsPattern[]]:=
	With[{
	PolygonDataLegendLabel=OptionValue[PolygonDataLegendLabel],
	PolygonDataLegendPlaced=OptionValue[PolygonDataLegendPlaced],
	PolygonDataLegendLabelMag=OptionValue[PolygonDataLegendLabelMag],
	PolygonDataLegendMarkerSize=OptionValue[PolygonDataLegendMarkerSize],
	PolygonDataLegendTickMag=OptionValue[PolygonDataLegendTickMag],
	ScalarDirectory=OptionValue[ScalarDirectory],
	ScalarParts=OptionValue[ScalarParts],
	PolygonColorFunction=OptionValue[PolygonColorFunction],
	PolygonColorScaled=OptionValue[PolygonColorScaled]}, 
	Module[{scalar, maxScalar},
		scalar=Extract[Import[file,ScalarDirectory],ScalarParts];
		maxScalar=If[PolygonColorScaled,Max[scalar],1];
		Placed[
			BarLegend[
				{PolygonColorFunction, {0.0, 1.0}}, 
				LegendLabel -> MaTeX[
					PolygonDataLegendLabel[[1]]<>
					If[PolygonColorScaled,
						"\\frac{"<>PolygonDataLegendLabel[[2]]<>"}{"<>NumberToTeXString[maxScalar]<>"}",
						PolygonDataLegendLabel[[2]]
					]<>
					PolygonDataLegendLabel[[3]], 
				Magnification -> PolygonDataLegendLabelMag], 
				LabelStyle -> Directive[Black, FontFamily -> "Latin Modern Roman", FontSize -> Ceiling[14*PolygonDataLegendTickMag]], 
				LegendMarkerSize -> PolygonDataLegendMarkerSize], 
		PolygonDataLegendPlaced]
		]
	]


(* ::Subsection:: *)
(*PolygonSwatchLegend*)


PolygonSwatchLegendOpts={PolygonDataLegendSwatchOffset->0, PolygonDataLegendSwatchSpacings->0};

Options[PolygonSwatchLegend]=Join[
						PolygonSwatchLegendOpts,
						PolygonDataLegendOpts,
						FilterRules[Options[PolygonColors],{ScalarDirectory,ScalarParts,PolygonColorFunction}]
						];

PolygonSwatchLegend[file_,opts:OptionsPattern[]]:=
	With[{
	PolygonDataLegendSwatchOffset=OptionValue[PolygonDataLegendSwatchOffset],
	PolygonDataLegendSwatchSpacings=OptionValue[PolygonDataLegendSwatchSpacings],
	PolygonDataLegendLabel=OptionValue[PolygonDataLegendLabel],
	PolygonDataLegendPlaced=OptionValue[PolygonDataLegendPlaced],
	PolygonDataLegendLabelMag=OptionValue[PolygonDataLegendLabelMag],
	PolygonDataLegendMarkerSize=OptionValue[PolygonDataLegendMarkerSize],
	PolygonDataLegendTickMag=OptionValue[PolygonDataLegendTickMag],
	ScalarDirectory=OptionValue[ScalarDirectory],
	ScalarParts=OptionValue[ScalarParts],
	PolygonColorFunction=OptionValue[PolygonColorFunction]}, 
	Module[{colors, labels, scalar, maxScalar},
		scalar=DeleteDuplicates[Extract[Import[file,ScalarDirectory],ScalarParts]];
		colors=Map[PolygonColorFunction, scalar];
		labels=Table[
				DisplayForm[AdjustmentBox[MaTeX[i, Magnification->PolygonDataLegendTickMag],BoxBaselineShift->PolygonDataLegendSwatchOffset]],
			{i,scalar}];
		Placed[
			SwatchLegend[
				colors,
				labels,
				LegendLabel->MaTeX[StringJoin[PolygonDataLegendLabel],Magnification->PolygonDataLegendLabelMag],
				LegendMarkerSize->PolygonDataLegendMarkerSize,
				LegendMarkers->Table[
								Graphics[{Opacity[1],EdgeForm[Directive[Thin,Black]],Rectangle[]}],
							{i,1,Length[colors]}],
				Spacings->PolygonDataLegendSwatchSpacings
				],
			PolygonDataLegendPlaced
			]
		]
	]


(* ::Subsection:: *)
(*End Matter*)


End[]


EndPackage[]
