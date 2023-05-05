(* ::Package:: *)

(* ::Section:: *)
(*PolygonPlots*)


BeginPackage["PolygonPlots`"]

ParseHDF5Polygons::usage = "ParseHDF5Polygons[file, opts] creates a list of polygons and disconnected polygons from the vertices in the input file."
	PolysDirectory::usage = "The directory in the HDF5 file for the polygon vertices."
	NPolysDisDirectory::usage = "The directory in the HDF5 file for the number of disconnected polygons."
	PolysDisDirectory::usage = "The directory in the HDF5 file for the disconnected polygon vertices."
	PolyOrGeoPoly::usage = "Whether to parse the vertices as Polygon or GeoPolygon."


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`
<<EurekaColors`
<<WLHelpers`
<<GeoTick`
<<WorldPolygons`
<<PolygonColorsLegends`
<<FancyGeoFrame`


(* ::Subsection:: *)
(*PlotPolygon*)


PlotPolygonOpts={PlotPolygonRange->{{-100,15},{-9,39}}, PlotPolygonImageSize->800, PlotPolygonABLegend->True,PlotPolygonDataLegend->"Bar"};
Options[PlotPolygon]=DeleteDuplicates[Join[
					PlotPolygonOpts,
					Options[ABLegend],
					Options[PolygonBarLegend],
					Options[PolygonSwatchLegend],
					Options[PolygonColors],
					Options[WorldTicks],
					Options[WorldPolygon]
					]];

PlotPolygon[file_, opts:OptionsPattern[]] :=
	With[{
	PlotPolygonRange=OptionValue[PlotPolygonRange], 
	PlotPolygonImageSize=OptionValue[PlotPolygonImageSize],
	PlotPolygonABLegend=OptionValue[PlotPolygonABLegend],
	PlotPolygonDataLegend=OptionValue[PlotPolygonDataLegend]}, 
	Module[{polyColor, polyColorDis, world, graphics},
		{polyColor, polyColorDis} = PolygonColors[file, DelegateOptions[opts, PlotPolygon]];
		world = WorldPolygon[DelegateOptions[opts, PlotPolygon]];
		graphics = Graphics[
					Join[polyColorDis, polyColor, world], 
					PlotRange -> PlotPolygonRange, 
					Frame -> True, 
					FrameTicks -> WorldTicks[DelegateOptions[opts, PlotPolygon]],
					FrameTicksStyle-> Directive[Black, 20], 
					PlotRangeClipping -> True, 
					FrameLabel -> {{None, None}, {None, None}}, 
					ImageSize -> PlotPolygonImageSize];
		If[PlotPolygonABLegend, graphics = Legended[graphics, ABLegend[DelegateOptions[opts, PlotPolygon]]]];
		graphics = Which[
			PlotPolygonDataLegend===None, graphics,
			PlotPolygonDataLegend=="Bar", Legended[graphics,PolygonBarLegend[file, DelegateOptions[opts, PlotPolygon]]],
			PlotPolygonDataLegend=="Swatch", Legended[graphics,PolygonSwatchLegend[file, DelegateOptions[opts, PlotPolygon]]]
			];
		graphics
	]
]


PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	AColor->Yellow,
	PlotPolygonABLegend->False,
	PlotPolygonDataLegend->"Bar",
	AEdgeForm->Directive[Thick,Black]
]


PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	ScalarDirectory->"parts_stat/spectral_P",
	PolygonColorScaled->False,
	PolygonColorTransform->Function[#],
	PolygonColorFunction->Function[If[#==1,Pink,Blue]],
	PlotPolygonDataLegend->"Swatch",
	PolygonDataLegendLabelMag->2,
	PolygonDataLegendPlaced->{0.9,0.7},
	PolygonDataLegendMarkerSize->{20,20},
	PolygonDataLegendTickMag->2,
	PolygonDataLegendSwatchOffset->0.6,
	PolygonDataLegendSwatchSpacings->0]


Options[PlotPolygon]//MatrixForm


(* ::Subsection:: *)
(*PlotPolygonSlices*)


PlotPolygonSlicesOpts={EndFrame->-1, SlicesReverseRowCol->False};
Options[PlotPolygonSlices]=Join[
					PlotPolygonSlicesOpts,
					Options[PlotPolygon]
					];

PlotPolygonSlices[file_, opts:OptionsPattern[]] :=
	With[{
	EndFrame=OptionValue[EndFrame],
	ScalarDirectory=OptionValue[ScalarDirectory],
	SlicesReverseRowCol=OptionValue[SlicesReverseRowCol],
	SP=OptionValue[ScalarParts]}, 
	Module[{length, frames = {}},
		length=Length[
				Extract[Import[file,ScalarDirectory], If[SlicesReverseRowCol,Reverse[SP],SP]]
			];
		Do[
			AppendTo[frames, PlotPolygon[file, ScalarParts->If[SlicesReverseRowCol,Reverse[{All,i}],{All,i}], DelegateOptions[opts, PlotPolygonSlices]]]
		,{i,1,If[EndFrame==-1,length,EndFrame]}];
		frames
	]
]


PlotPolygonSlices["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	ScalarDirectory->"tpt_nonstat/statistics/normalized_reactive_density",
	AColor->Yellow,
	ScalarParts->{All,1},
	PolygonDataLegendPlaced->{0.5,0.5},
	EndFrame->4]


(* ::Subsection:: *)
(*PlotGeoPolygon*)


PlotGeoPolygonOpts={
	PlotGeoPolygonTicks->{{-100,0, 15},{-9,0, 39}}, 
	PlotGeoPolygonGeoProjection->"Equirectangular", 
	PlotGeoPolygonGeoBackground->None, 
	PlotGeoPolygonImageSize->800, 
	PlotGeoPolygonABLegend->True, 
	PlotGeoPolygonDataLegend->"Bar"};
Options[PlotGeoPolygon]=DeleteDuplicates[Join[
					PlotGeoPolygonOpts,
					Options[ABLegend],
					Options[PolygonBarLegend],
					Options[PolygonSwatchLegend],
					Options[PolygonColors],
					Options[WorldPolygon],
					Options[FancyGeoFrame]
					]];

PlotGeoPolygon[file_, opts:OptionsPattern[]] :=
	With[{
	PlotGeoPolygonTicks=OptionValue[PlotGeoPolygonTicks],
	PlotGeoPolygonGeoProjection=OptionValue[PlotGeoPolygonGeoProjection],
	PlotGeoPolygonGeoBackground=OptionValue[PlotGeoPolygonGeoBackground],
	PlotGeoPolygonImageSize=OptionValue[PlotGeoPolygonImageSize],
	PlotGeoPolygonABLegend=OptionValue[PlotGeoPolygonABLegend],
	PlotGeoPolygonDataLegend=OptionValue[PlotGeoPolygonDataLegend]}, 
	Module[{frame,newrange,polyColor, polyColorDis, world, graphics},
		{polyColor, polyColorDis} = PolygonColors[file, PolyOrGeoPoly->"GeoPolygon",DelegateOptions[opts, PlotGeoPolygon]];
		{frame,newrange} = FancyGeoFrame[PlotGeoPolygonTicks[[1]],PlotGeoPolygonTicks[[2]], DelegateOptions[opts, PlotGeoPolygon]];
		world = WorldGeoPolygon[DelegateOptions[opts, PlotPolygon]];
		graphics = GeoGraphics[
						Join[polyColorDis, polyColor,world,frame],
						GeoRange->Reverse[newrange],
						GeoRangePadding->{None,None},
						GeoBackground->PlotGeoPolygonGeoBackground,
						GeoGridLines->None,
						GeoProjection->PlotGeoPolygonGeoProjection,
						ImageSize->PlotGeoPolygonImageSize
					];
		If[PlotGeoPolygonABLegend, graphics = Legended[graphics, ABLegend[DelegateOptions[opts, PlotGeoPolygon]]]];
		graphics = Which[
			PlotGeoPolygonDataLegend===None, graphics,
			PlotGeoPolygonDataLegend=="Bar", Legended[graphics,PolygonBarLegend[file, DelegateOptions[opts, PlotGeoPolygon]]],
			PlotGeoPolygonDataLegend=="Swatch", Legended[graphics,PolygonSwatchLegend[file, DelegateOptions[opts, PlotGeoPolygon]]]
			];
		graphics
	]
]


PlotGeoPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	PlotGeoPolygonGeoProjection->"Albers",
	PlotGeoPolygonGeoBackground->GeoStyling["ContourMap",Contours->4],
	AEdgeForm->Directive[Thick,Black],
	BEdgeForm->Directive[Black],
	PolygonColorEdgeForm->None]


(* ::Subsection:: *)
(*PlotGeoPolygonSlices*)


PlotGeoPolygonSlicesOpts={EndFrame->-1, SlicesReverseRowCol->False};
Options[PlotGeoPolygonSlices]=Join[
					PlotGeoPolygonSlicesOpts,
					Options[PlotGeoPolygon]
					];

PlotGeoPolygonSlices[file_, opts:OptionsPattern[]] :=
	With[{
	EndFrame=OptionValue[EndFrame],
	ScalarDirectory=OptionValue[ScalarDirectory],
	SlicesReverseRowCol=OptionValue[SlicesReverseRowCol],
	SP=OptionValue[ScalarParts]}, 
	Module[{length, frames = {}},
		length=Length[
				Extract[Import[file,ScalarDirectory], If[SlicesReverseRowCol,Reverse[SP],SP]]
			];
		Do[
			AppendTo[frames, PlotGeoPolygon[file, ScalarParts->If[SlicesReverseRowCol,Reverse[{All,i}],{All,i}], DelegateOptions[opts, PlotPolygonSlices]]]
		,{i,1,If[EndFrame==-1,length,EndFrame]}];
		frames
	]
]


Quit[]


PlotGeoPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	PlotGeoPolygonGeoProjection->"Albers",
	PlotGeoPolygonGeoBackground->GeoStyling["ContourMap",Contours->4],
	AEdgeForm->Directive[Thick,Black],
	BEdgeForm->Directive[Black],
	PolygonColorEdgeForm->None]


PlotGeoPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	PlotGeoPolygonGeoProjection->"Equirectangular"]


PlotGeoPolygonSlices["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	ScalarDirectory->"tpt_nonstat/statistics/normalized_reactive_density",
	ScalarParts->{All,1},
	EndFrame->4,
	PlotGeoPolygonGeoProjection->"Albers",
	PlotGeoPolygonGeoBackground->GeoStyling["ContourMap",Contours->4],
	AEdgeForm->Directive[Thick,Black],
	BEdgeForm->Directive[Black],
	PolygonColorEdgeForm->None]


Options[PlotGeoPolygon]//MatrixForm
