(* ::Package:: *)

(* ::Section:: *)
(*PolygonPlots*)


BeginPackage["PolygonPlots`"]

PlotPolygon::usage = "PlotPolygon[file,opts] plots Polygons using Graphics."
	PlotPolygonRange::usage = "{{xmin,xmax},{ymin,ymax}}"
	PlotPolygonImageSize::usage = "The image size of the Graphics object."
	PlotPolygonABLegend::usage = "Boolean, whether to show the ABLegend."
	PlotPolygonDataLegend::usage = "If None, show no DataLegend. If \"Bar\", use a bar legend. If \"Swatch\" use a swatch legend. "
	
PlotPolygonSlices::usage = "PlotPolygonSlices[file, opts] plots frames of an array as Polygons using Graphics."
	EndFrame::usage = "If -1, plot every frame of the array, otherwise plot up to and including the value of EndFrame."
	SlicesReverseRowCol::usage = "Boolean, whether to transpose the array. Used to take slices along rows vs. columns."
	
PlotGeoPolygon::usage = "PlotGeoPolygon[file,opts] plots GeoPolygons using GeoGraphics."
	PlotGeoPolygonTicks::usage = "{xTicks,yTicks}. The range of the plot is controlled by the min/max values of the ticks."
	PlotGeoPolygonGeoProjection::usage = "The GeoProjection used by GeoGraphics."
	PlotGeoPolygonGeoBackground::usage = "The GeoBackground used by GeoGraphics."
	PlotGeoPolygonImageSize::usage = "The image size of the Graphics object."
	PlotGeoPolygonABLegend::usage = "Boolean, whether to show the ABLegend."
	PlotGeoPolygonDataLegend::usage = "If None, show no DataLegend. If \"Bar\", use a bar legend. If \"Swatch\" use a swatch legend. "
	
PlotGeoPolygonSlices::usage = "PlotGeoPolygonSlices[file, opts] plots frames of an array as GeoPolygons using GeoGraphics."
	EndFrame::usage = "If -1, plot every frame of the array, otherwise plot up to and including the value of EndFrame."
	SlicesReverseRowCol::usage = "Boolean, whether to transpose the array. Used to take slices along rows vs. columns."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`
<<EurekaColors`
<<WLHelpers`
<<GeoTick`
<<WorldPolygons`
<<PolygonColorsLegends`
<<FancyGeoFrame`


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection:: *)
(*End Matter*)


End[]


EndPackage[]


(*This ensures that all the symbols defined in the dependencies get exported.*)
BeginPackage["PolygonPlots`",{"MaTeX`","EurekaColors`","WLHelpers`","GeoTick`","WorldPolygons`","PolygonColorsLegends`","FancyGeoFrame`"}]
EndPackage[]
