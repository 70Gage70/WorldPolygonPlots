(* ::Package:: *)

(* ::Section:: *)
(*TPTPolygonPlots*)


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<PolygonPlots`


Options[PlotPolygon]//MatrixForm
Options[PlotGeoPolygon]//MatrixForm


(* ::Subsection:: *)
(*Templates*)


PPOpts=Options[PlotPolygon];


myopts={
	WorldTicksX->{-100, 0, 15},
	WorldTicksY->{-9,0,39},
	AvoidColor->None,
	PolygonDataLegendPlaced->{0.9, 0.65},
	PolygonDataLegendLabelMag->1.4,
	PolygonDataLegendMarkerSize->{15,150},
	PlotPolygonImageSize->1000};
	
NorthAmericaGDPTPTPolygon=ReplaceRules[PPOpts, myopts];


myopts={
	WorldTicksX->{-100, 0, 15},
	WorldTicksY->{-9,0,39},
	ScalarDirectory->"parts_stat/spectral_P",
	PolygonColorScaled->False,
	PolygonColorTransform->Function[#],
	PolygonColorFunction->Function[If[#==1,Hue[0.5,0.5,0.5],Blue]],
	PolygonColorEdgeForm->None,
	AEdgeForm->None,
	BEdgeForm->None,
	PolygonDataLegendLabel->"\\text{parts.}",
	PlotPolygonDataLegend->"Swatch",
	PolygonDataLegendLabelMag->2,
	PolygonDataLegendPlaced->{0.9,0.7},
	PolygonDataLegendMarkerSize->{20,20},
	PolygonDataLegendTickMag->2,
	PolygonDataLegendSwatchOffset->0.6,
	PolygonDataLegendSwatchSpacings->0,
	AvoidColor->None,
	PlotPolygonImageSize->1000};
	
NorthAmericaGDPPartitionsPolygon=ReplaceRules[PPOpts, myopts];


PGPOpts=Options[PlotGeoPolygon];


myopts={
	PlotGeoPolygonTicks->{{-100,-85,-70,-55,-40,-25,-10,5,15},{-9,1,11,21,31,39}},
	FancyTicksPadding->{8,13},
	PlotGeoPolygonGeoProjection->"Albers",
	PlotGeoPolygonGeoBackground->GeoStyling["ContourMap",Contours->4],
	AEdgeForm->Directive[Thick,Black],
	BEdgeForm->Directive[Black],
	PolygonColorEdgeForm->None,
	ABPlaced->{0.077,0.8},
	FancyBoxesSize->{{2,2},{2,1.5}},
	FancyTicksAngle->{-22,-20}*Pi/180,
	FancyTicksYDelta->2.5};
	
NorthAmericaGDPTPTGeoPolygon=ReplaceRules[PGPOpts, myopts];


(* ::Subsection:: *)
(*File In*)


fileIN="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/tptfromulamtest.h5";


(* ::Subsection:: *)
(*PlotPolygon*)


PlotPolygon[fileIN,NorthAmericaGDPTPTPolygon]


PlotPolygon[fileIN,ScalarDirectory->"parts_stat/spectral_f",NorthAmericaGDPPartitionsPolygon]
PlotPolygon[fileIN,ScalarDirectory->"parts_nonstat/spectral_P_plus",NorthAmericaGDPPartitionsPolygon]


(* ::Subsection:: *)
(*PlotPolygonSlices*)


PlotPolygonSlices[fileIN,
	ScalarDirectory->"tpt_nonstat/statistics/normalized_reactive_density",
	EndFrame->2,
	NorthAmericaGDPTPTPolygon]


(* ::Subsection:: *)
(*PlotGeoPolygon*)


PlotGeoPolygon[fileIN,NorthAmericaGDPTPTGeoPolygon]


(* ::Subsection:: *)
(*PlotGeoPolygonSlices*)


PlotGeoPolygonSlices[fileIN,
	ScalarDirectory->"tpt_nonstat/statistics/normalized_reactive_density",
	EndFrame->2,
	NorthAmericaGDPTPTGeoPolygon]
