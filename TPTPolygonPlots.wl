(* ::Package:: *)

(* ::Section:: *)
(*TPTPolygonPlots*)


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<MaTeX`
<<EurekaColors`
<<WLHelpers`
<<GeoTick`
<<WorldPolygons`
<<PolygonColors`


UlamScalars=<|
	"counts"->"\\text{counts}"
	|>;

TPTScalars=
	<|
	"normalized_reactive_density"->"\\mu^{AB}",
	"density"->"P \\cdot i_0",
	"pi_stationary"->"\\pi",
	"remaining_time"->"t^{i \\mathbb{B}}",
	"q_plus"->"q^{+}"
	|>;


ParseABInds["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5"]


Graphics[PolygonColors["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5", AColor->Yellow, PolygonOpacity->0.9][[1]]]


GeoGraphics[PolygonColors["/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPTparts.h5", AColor->Yellow, PolygonOpacity->0.9, PolyOrGeoPoly->"GeoPolygon",PolygonColorTransform->Function[#^(1/8)]][[1]],GeoProjection->"Albers"]


(* ::Subsection:: *)
(*ABLegend*)


ABLegendOpts={ABPlaced->{0.077,0.18}, ABLegendMag->1.8};
Options[ABLegend]=Join[
					ABLegendOpts,
					FilterRules[Options[PolygonColors],{AColor,BColor,DisconColor,AvoidColor}]];

ABLegend[opts:OptionsPattern[]]:=
	With[{AColor = OptionValue[AColor], BColor = OptionValue[BColor], DisconColor = OptionValue[DisconColor], AvoidColor = OptionValue[AvoidColor], ABPlaced = OptionValue[ABPlaced], ABLegendMag = OptionValue[ABLegendMag]}, 
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
				LegendMarkerSize->12,
				LegendMarkers->Table[
								Graphics[{Opacity[1],EdgeForm[Directive[Thin,Black]],Rectangle[]}],
							{i,1,Length[colors]}]
				],
			ABPlaced
			]
		]
	]


(* ::Subsection:: *)
(*ScalarLegend*)


ScalarLegendOpts={ScalarLegendLabel->"normalized_reactive_density", ScalarLegendPlaced->{0.9, 0.8}, ScalarLegendLabelMag->1.2, ScalarLegendMarkerSize->{10,100}, ScalarLegendTickFontSize->14};
Options[ScalarLegend]=Join[
						ScalarLegendOpts,
						FilterRules[PolygonColorsOpts,PolygonColorFunction],
						FilterRules[PolygonColorsOpts,Scalar],
						FilterRules[PolygonColorsOpts,PlotExponent]
						];

ScalarLegend[scalar_,opts:OptionsPattern[]]:=
	With[
	{ScalarLegendLabel=OptionValue[ScalarLegendLabel],ScalarLegendPlaced=OptionValue[ScalarLegendPlaced],ScalarLegendLabelMag=OptionValue[ScalarLegendLabelMag],
	ScalarLegendMarkerSize=OptionValue[ScalarLegendMarkerSize],ScalarLegendTickFontSize=OptionValue[ScalarLegendTickFontSize],
	PolygonColorFunction=OptionValue[PolygonColorFunction], PlotExponent= OptionValue[PlotExponent]}, 
	Module[{maxScalar = Max[scalar]},
		Placed[
			BarLegend[
				{PolygonColorFunction, {0.0, 1.0}}, 
				LegendLabel -> MaTeX[
					"\\left(\\frac{" <> 
					Which[
						MemberQ[Keys[TPTScalars],ScalarLegendLabel], TPTScalars[ScalarLegendLabel],
						MemberQ[Keys[UlamScalars],ScalarLegendLabel], UlamScalars[ScalarLegendLabel]
					] <> 
					"}{" <> 
					NumberToTeXString[maxScalar] <> 
					"}\\right)^{" <> 
					FractionToInlineLaTeX[PlotExponent] <>
					"}", 
					Magnification -> ScalarLegendLabelMag], 
				LabelStyle -> Directive[Black, FontFamily -> "Latin Modern Roman", FontSize -> ScalarLegendTickFontSize], 
				LegendMarkerSize -> ScalarLegendMarkerSize], 
		ScalarLegendPlaced]
		]
	]


(* ::Subsection:: *)
(*PlotScalar*)


PlotScalarOpts={WorldRange->{{-100,-59},{6,32}}, PlotScalarImageSize->800};
Options[PlotScalar]=Join[
					PlotScalarOpts,
					ScalarLegendOpts,
					Options[PolygonColors],
					Options[WorldTicks],
					Options[WorldPolygon]
					];

PlotScalar[fulam_, scalar_, opts:OptionsPattern[]] :=
	With[{WorldRange=OptionValue[WorldRange], PlotExponent=OptionValue[PlotExponent], PlotScalarImageSize=OptionValue[PlotScalarImageSize]}, 
	Module[{polyColor, polyColorDis, world},
		{polyColor, polyColorDis} = PolygonColors[fulam, scalar, InheritOpts[PlotScalar,PolygonColors,opts]];
		world = WorldPolygon[InheritOpts[PlotScalar,WorldPolygon,opts]];
		Legended[
			Legended[
				Graphics[
					Join[polyColor, polyColorDis, world], 
					PlotRange -> WorldRange, 
					Frame -> True, 
					FrameTicks -> WorldTicks[InheritOpts[PlotScalar,WorldTicks,opts]],
					FrameTicksStyle-> Directive[Black, 20], 
					PlotRangeClipping -> True, 
					FrameLabel -> {{None, None}, {None, None}}, 
					ImageSize -> PlotScalarImageSize], 
			ABLegend[InheritOpts[PlotScalar,ABLegend,opts]]
			], 
		ScalarLegend[scalar, InheritOpts[PlotScalar,ScalarLegend,opts]]
		]
	]
]


(* ::Subsection:: *)
(*PlotScalarSlices*)


PlotScalarSlicesOpts={EndFrame->-1};
Options[PlotScalarSlices]=Join[
					PlotScalarSlicesOpts,
					Options[PlotScalar]
					];

PlotScalarSlices[fulam_, scalar_, opts:OptionsPattern[]] :=
	With[{EndFrame=OptionValue[EndFrame]}, 
	Module[{slice, frames = {}},
		Do[
			slice=scalar[[All,i]];
			AppendTo[frames, PlotScalar[fulam, slice,InheritOpts[PlotScalarSlices,PlotScalar,opts]]]
		,{i,1,If[EndFrame==-1,Dimensions[scalar][[2]],EndFrame]}];
		frames
	]
]


(* ::Section:: *)
(*Calculations*)


Options[PlotScalar]//Sort//MatrixForm


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";
scalarmu=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
scalarpi=Import[ftpt,"/tpt_homog/statistics/pi_stationary"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulamTPT.h5";

Graphics[ParseHDF5Polygons[fulam]]
s=PlotScalar[fulam, scalarmu,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	ScalarLegendLabel->"normalized_reactive_density", PlotExponent->1/8, ScalarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}, AvoidColor->LightGray]
s=PlotScalar[fulam, scalarpi,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	ScalarLegendLabel->"pi_stationary", PlotExponent->1/8, ScalarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}]


Polygon[{{10.9, 0.6}, {10.9, 3.8}, {14.2, 3.8}, {14.2, 0.6}}]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_stat_test.h5";
scalarmu=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
scalarpi=Import[ftpt,"/tpt_homog/statistics/pi_stationary"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";

Graphics[ParseHDF5Polygons[fulam]]
s=PlotScalar[fulam, scalarmu,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	ScalarLegendLabel->"normalized_reactive_density", PlotExponent->1/8, ScalarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}, AvoidColor->LightGray]
s=PlotScalar[fulam, scalarpi,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	ScalarLegendLabel->"pi_stationary", PlotExponent->1/8, ScalarLegendPlaced->{0.9,0.6}, 
	ABPlaced->{0.077,0.22}]


Export["/Users/gagebonner/Desktop/mu-stat.png",s]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_nonstat_test.h5";
scalarMU=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";
framesMU=PlotScalarSlices[fulam, scalarMU, 
	EndFrame->-1,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, ScalarLegendLabel->"normalized_reactive_density", PlotExponent->1/6, ScalarLegendPlaced->{0.9,0.6}, ABPlaced->{0.077,0.22}, AvoidColor->None];
ListAnimate[framesMU,AnimationRunning->False]



Export["/Users/gagebonner/Desktop/mu-nonstat.gif",framesMU]


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_nonstat_test.h5";
scalarDEN=Import[ftpt,"/tpt_homog/statistics/density"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";
framesDEN=PlotScalarSlices[fulam, scalarDEN, 
	EndFrame->-1,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, ScalarLegendLabel->"density", PlotExponent->1/6, ScalarLegendPlaced->{0.9,0.6}, ABPlaced->{0.077,0.22}, AvoidColor->None];
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
s=PlotScalar[fulam, scalarcounts,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, 
	ScalarLegendLabel->"counts", PlotExponent->1/2, ScalarLegendPlaced->{0.9,0.6}, ScalarLegendLabelMag->1.2, 
	ABPlaced->{0.077,0.18}, ABLegendMag->1.8,
	AvoidColor->None]


Options[PlotScalar]//MatrixForm


Export["/Users/gagebonner/Desktop/Rem Time Paper/new fgs/pdfs/tABboxT_vor.pdf",utVORcrop,Background->None];
