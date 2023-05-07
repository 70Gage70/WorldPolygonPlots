(* ::Package:: *)

(* ::Section:: *)
(*TPTPolygonPlots*)


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<PolygonPlots`


Options[PlotPolygon]//MatrixForm


(* ::Subsection:: *)
(*Templates*)


allopts=Options[PlotPolygon];


myopts={
	WorldTicksX->{-100, 0, 15},
	WorldTicksY->{-9,0,39},
	AvoidColor->None,
	PolygonDataLegendPlaced->{0.9, 0.65},
	PolygonDataLegendLabelMag->1.4,
	PolygonDataLegendMarkerSize->{15,150},
	PlotPolygonImageSize->1000};
	
NorthAmericaGDPTPT=ReplaceRules[allopts, myopts];


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
	
NorthAmericaGDPPartitions=ReplaceRules[allopts, myopts];


PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",NorthAmericaGDPTPT]


PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",ScalarDirectory->"parts_stat/spectral_P",NorthAmericaGDPPartitions]
PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",ScalarDirectory->"parts_stat/spectral_f",NorthAmericaGDPPartitions]
PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",ScalarDirectory->"parts_stat/hitting_location",NorthAmericaGDPPartitions]
PlotPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",ScalarDirectory->"parts_nonstat/spectral_P_plus",NorthAmericaGDPPartitions]


(* ::Subsection:: *)
(*PlotPolygonSlices*)


PlotPolygonSlices["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	ScalarDirectory->"tpt_nonstat/statistics/normalized_reactive_density",
	EndFrame->4,
	NorthAmericaGDPTPT]


(* ::Subsection:: *)
(*PlotGeoPolygon*)


PlotGeoPolygon["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5",
	PlotGeoPolygonGeoProjection->"Albers",
	PlotGeoPolygonGeoBackground->GeoStyling["ContourMap",Contours->4],
	AEdgeForm->Directive[Thick,Black],
	BEdgeForm->Directive[Black],
	PolygonColorEdgeForm->None]


(* ::Subsection:: *)
(*PlotGeoPolygonSlices*)


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


(* ::Section:: *)
(*Calculations*)


Options[PlotPolygon]//Sort//MatrixForm


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";
scalarmu=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
scalarpi=Import[ftpt,"/tpt_homog/statistics/pi_stationary"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";

Graphics[ParseHDF5Polygons[fulam]]
s=PlotPolygon[fulam, scalarmu,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	PolygonBarLegendLabel->"normalized_reactive_density", PlotExponent->1/8, PolygonBarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}, AvoidColor->LightGray]
s=PlotPolygon[fulam, scalarpi,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	PolygonBarLegendLabel->"pi_stationary", PlotExponent->1/8, PolygonBarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}]


Polygon[{{10.9, 0.6}, {10.9, 3.8}, {14.2, 3.8}, {14.2, 0.6}}]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_stat_test.h5";
scalarmu=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
scalarpi=Import[ftpt,"/tpt_homog/statistics/pi_stationary"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";

Graphics[ParseHDF5Polygons[fulam]]
s=PlotPolygon[fulam, scalarmu,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	PolygonBarLegendLabel->"normalized_reactive_density", PlotExponent->1/8, PolygonBarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}, AvoidColor->LightGray]
s=PlotPolygon[fulam, scalarpi,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	PolygonBarLegendLabel->"pi_stationary", PlotExponent->1/8, PolygonBarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}]


Export["/Users/gagebonner/Desktop/mu-stat.png",s]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_nonstat_test.h5";
scalarMU=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";
framesMU=PlotPolygonSlices[fulam, scalarMU, 
	EndFrame->-1,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, PolygonBarLegendLabel->"normalized_reactive_density", PlotExponent->1/6, PolygonBarLegendPlaced->{0.9,0.6}, ABPlaced->{0.077,0.22}, AvoidColor->None];
ListAnimate[framesMU,AnimationRunning->False]



Export["/Users/gagebonner/Desktop/mu-nonstat.gif",framesMU]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_nonstat_test.h5";
scalarDEN=Import[ftpt,"/tpt_homog/statistics/density"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";
framesDEN=PlotPolygonSlices[fulam, scalarDEN, 
	EndFrame->-1,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, PolygonBarLegendLabel->"density", PlotExponent->1/6, PolygonBarLegendPlaced->{0.9,0.6}, ABPlaced->{0.077,0.22}, AvoidColor->None];
ListAnimate[framesDEN,AnimationRunning->False]


Export["/Users/gagebonner/Desktop/push-forward.gif",framesDEN]


famesCOMP=Table[Grid[{{framesMU[[i]]},{framesDEN[[i]]}}],{i,1,Length[framesMU]}];


Export["/Users/gagebonner/Desktop/mu-comp.gif",famesCOMP]


(* ::Section:: *)
(*Under Construction*)


FCurrent[fulam_,ftpt_]:=Module[{centers,fplus,ci,fi,fres},
centers=Transpose[Import[fulam,"/ulam/polys_centers"]];
fplus=Transpose[Import[ftpt,"/tpt/f+"]];
fres={};
Do[
ci=centers[[i]];
fi=Sum[fplus[[i]][[j]]*Normalize[centers[[j]]-ci],{j,1,Length[centers]}];
AppendTo[fres,{ci,Norm[fi],Normalize[fi]}];
,{i,1,Length[centers]}];
Return[fres]
]


PlotVector[fulam_,ftpt_,nbins_,ArrowSizeScale_,ArrowLengthScale_,hue_,brightness_]:=Module[{fres,scaledColor,scaledColormin,scaledColormax,arrow,binAssignments,binvals,arrowColor,arrowhead,arrows},
fres=FCurrent[fulam,ftpt];
arrows={};
binAssignments=ClusteringComponents[fres[[All,2]],nbins,Method->"KMeans"];
binvals=Table[Mean[fres[[All,2]][[Flatten[Position[binAssignments,i]]]]],{i,1,nbins}];
scaledColor=Table[binvals[[binAssignments[[i]]]],{i,1,Length[fres]}];
scaledColormin=Min[scaledColor];
scaledColormax=Max[scaledColor];
scaledColor=Table[If[scaledColor[[i]]==scaledColormin,0.0,(scaledColor[[i]]/scaledColormax)^(1/2)],{i,1,Length[fres]}];
Do[
If[scaledColor[[i]]!=0.0,
arrow=Arrow[{fres[[i,1]],fres[[i,1]]+ArrowLengthScale*fres[[i,3]]}];
arrowColor=Hue[hue,scaledColor[[i]],brightness];
arrowhead=Arrowheads[ArrowSizeScale*0.04];
AppendTo[arrows,{arrowColor,arrowhead,arrow}]
]
,{i,1,Length[fres]}];
arrows = Flatten[arrows];
Return[{arrows,scaledColor,binvals}]
]


PlotMUFLUX[fulam_,ftpt_,opacity_,nbins_,hutFtF_]:=Module[{CFtF,tremplot,plotvec,max},
CFtf[hue_]:=Hue[hutFtF,hue,1.0];
tremplot=PlotMU[fulam,ftpt,opacity];
plotvec=PlotVector[fulam,ftpt,nbins,0.5,1,hutFtF,1.0];
max=Max[plotvec[[3]]];
u=Legended[
Show[tremplot,Graphics[plotvec[[1]]]],
Placed[
BarLegend[{CFtf,{0.1,1}},LegendLabel->MaTeX["\\left(\\frac{f_{+}^{AB}}{"<>NumberToTeXString[max]<>"}\\right)^{1/2}",Magnification->1.2],LabelStyle->Directive[Black,FontFamily->"Latin Modern Roman",FontSize->12],LegendMarkerSize->{10, 100}],
{0.75, 0.8}]
]
]


PlotCDF[ftpt_]:=Module[{dist,cdf,quants,ts,pts,mag,DistTicksY,DistTicksX,plot},
dist=Import[ftpt,"Data"]["/tpt/tAB_dist"];
cdf=Table[Sum[dist[[i]],{i,1,k}],{k,1,Length[dist]}];
quants={0.01,0.10,0.50,0.90,0.95};
ts=Flatten[Table[FirstPosition[cdf,_?(#>=quants[[i]]&)],{i,1,Length[quants]}]];
pts=Table[{ts[[i]],quants[[i]]},{i,1,Length[quants]}];
mag=1.6;
DistTicksY={{0.01,MaTeX["1\\%",Magnification->mag],{0,0.01}},{0.1,MaTeX["10\\%",Magnification->mag],{0,0.01}},{0.5,MaTeX["50\\%",Magnification->mag],{0,0.01}},{0.90,MaTeX["90\\%",Magnification->mag],{0,0.01}},{0.95,MaTeX["95\\%",Magnification->mag],{0,0.01}}};
DistTicksX=Table[{ts[[i]],Rotate[MaTeX[NumberToTeXStringDec[5*ts[[i]]/30.0],Magnification->mag],-\[Pi]/2],{0,0.01}},{i,1,Length[quants]}];
plot=ListPlot[
cdf,
	Joined->True,
	PlotRange->{{0,ts[[-1]]+1},{0,1.0}},
	PlotStyle->Directive[Thick,Black],
	FrameTicks->{{DistTicksY,None},{DistTicksX,None}},
	PlotRangePadding->{None,0.01},
	Frame->{True,True,False,False},
	FrameLabel->{MaTeX["t\\,\\text{(months)}",Magnification->mag],MaTeX["\\text{Prob}(t \\leq \\cdot)",Magnification->mag]},
	Epilog->{Gray,AbsolutePointSize[6],Dashed,Line[{{#[[1]],0},#,{0,#[[2]]}}]&/@pts},ImageSize->800];
plot
]


(* ::Section:: *)
(*Extras*)


x0=Flatten[Import["/Users/gagebonner/Desktop/Repositories/UlamMethod.jl/src/data/x0x5-NA-undrogued.h5","/x0"]];
y0=Flatten[Import["/Users/gagebonner/Desktop/Repositories/UlamMethod.jl/src/data/x0x5-NA-undrogued.h5","/y0"]];
xT=Flatten[Import["/Users/gagebonner/Desktop/Repositories/UlamMethod.jl/src/data/x0x5-NA-undrogued.h5","/xT"]];
yT=Flatten[Import["/Users/gagebonner/Desktop/Repositories/UlamMethod.jl/src/data/x0x5-NA-undrogued.h5","/yT"]];


trajLength=Table[Sqrt[(x0[[i]]-xT[[i]])^2+(y0[[i]]-yT[[i]])^2],{i,1,Length[x0]}];


HistogramList


Histogram[
	trajLength,
	{0, 3, 0.1},
	Frame->{{True,False},{True,False}},
	FrameTicks->{
		{{0,MaTeX["0",Magnification->2],{0,0.01}},{1,MaTeX["1",Magnification->2],{0,0.01}}, {2,MaTeX["2",Magnification->2],{0,0.01}}, {3,MaTeX["3",Magnification->2],{0,0.01}}},
		{{0,MaTeX["0",Magnification->2],{0,0.01}},{10^4,MaTeX["1 \\times 10^4",Magnification->2],{0,0.01}}, {2*10^4,MaTeX["2\\times 10^4",Magnification->2],{0,0.01}},{3*10^4,MaTeX["3\\times 10^4",Magnification->2],{0,0.01}},{4*10^4,MaTeX["4\\times 10^4",Magnification->2],{0,0.01}}}
	},
	PlotRange->{{0.06,3},{0,40000}},
	FrameLabel->{
		{MaTeX["\\text{Counts}", Magnification->2], None},
		{MaTeX["||\\vec{x}_0 - \\vec{x}_T||", Magnification->2], None}
	},
	ChartStyle->{Directive[LightGray,EdgeForm[Black]]},
	ImageSize->800
]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";
scalarmu=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
scalarpi=Import[ftpt,"/tpt_homog/statistics/pi_stationary"];
scalarcounts=Import[ftpt,"/ulam/counts"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";

Graphics[ParseHDF5Polygons[fulam]]
s=PlotPolygon[fulam, scalarcounts,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	PolygonBarLegendLabel->"counts", PlotExponent->1/2, PolygonBarLegendPlaced->{0.9,0.6}, PolygonBarLegendLabelMag->1.2, 
	ABPlaced->{0.077,0.18}, ABLegendMag->1.8,
	AvoidColor->None]


Options[PlotPolygon]//MatrixForm


Export["/Users/gagebonner/Desktop/Rem Time Paper/new fgs/pdfs/tABboxT_vor.pdf",utVORcrop,Background->None];
