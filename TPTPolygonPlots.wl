(* ::Package:: *)

(* ::Section:: *)
(*TPTPolygonPlots*)


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<MaTeX`
<<EurekaColors`
<<WLHelpers`


UlamScalars=<|
	"counts"->"\\text{cts.}"
	|>;

TPTScalars=
	<|
	"normalized_reactive_density"->"\\mu^{AB}",
	"density"->"P \\cdot i_0",
	"pi_stationary"->"\\pi",
	"remaining_time"->"t^{i \\mathbb{B}}",
	"q_plus"->"q^{+}"
	|>;


(* ::Subsection:: *)
(*ABLegend*)


ABLegendOpts={AColor->Hue[0.83,0.68,1.0], BColor->Red, DisconColor->Black, AvoidColor->LightGray, ABPlaced->{0.077,0.18}, ABLegendMag->1.8};
Options[ABLegend]=ABLegendOpts;

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
								Graphics[{EdgeForm[Directive[Thin,Black]],Rectangle[]}],
							{i,1,Length[colors]}]
				],
			ABPlaced
			]
		]
	]


(* ::Subsection:: *)
(*WorldTicks and WorldPolygon*)


WorldTicksOpts={WorldTicksX->{-100,-80,-59}, WorldTicksY->{6,20,32}, WorldTicksMag->1.8};
Options[WorldTicks]=WorldTicksOpts;

WorldTicks[opts:OptionsPattern[]]:=
	With[{WorldTicksX=OptionValue[WorldTicksX], WorldTicksY=OptionValue[WorldTicksY], WorldTicksMag=OptionValue[WorldTicksMag]},
	Module[{x,y,xEW,yNS},
		xEW[xt_]:=Which[xt<0, "^\\circ \\text{W}", xt>0, "^\\circ \\text{E}", xt==0, "^\\circ"];
		yNS[yt_]:=Which[yt<0, "^\\circ \\text{S}", yt>0, "^\\circ \\text{N}", yt==0, "^\\circ"];
		x=Table[{xt,MaTeX[ToString[Abs[xt]]<>xEW[xt],Magnification->WorldTicksMag],{0,0.01}},
			{xt,WorldTicksX}
		];
		y=Table[{yt,MaTeX[ToString[Abs[yt]]<>yNS[yt],Magnification->WorldTicksMag],{0,0.01}},
			{yt,WorldTicksY}
		];
		{{y,None},{x,None}}
		]
	]

WorldPolygonOpts={WorldOpacity->1, WorldColor->RGBColor[0.65, 0.65, 0.65]};
Options[WorldPolygon]=WorldPolygonOpts;

WorldPolygon[opts:OptionsPattern[]]:=
	With[{WorldOpacity=OptionValue[WorldOpacity], WorldColor=OptionValue[WorldColor]},
		{Opacity[WorldOpacity], FaceForm[WorldColor], EdgeForm[None], CountryData["World", "Polygon"]}
	]


(* ::Subsection:: *)
(*Parsing Polygons and TPT Indices*)


ParseHDF5Polygons[fulam_]:=
	Module[{polysin, vcells, polysdisin, vcellsdis},
		polysin=Transpose[Import[fulam, "/ulam/polys"]];
		vcells=Table[Polygon[Select[polysin, #[[3]] == i &][[All, 1;;2]]], {i, 1, Max[polysin[[All, 3]]]}];
		vcellsdis=If[Import[fulam,"/ulam/n_polys_dis"]==0,
			{},
			polysdisin=Transpose[Import[fulam,"/ulam/polys_dis"]];
			Table[Polygon[Select[polysdisin, #[[3]] == i &][[All, 1;;2]]], {i, 1, Max[polysdisin[[All, 3]]]}]
			];
		{vcells, vcellsdis}
	]

ParseABInds[ftpt_]:=
	Module[
	{
	Ainds = Import[ftpt, "/tpt_homog/indices/A"], 
	Binds = Import[ftpt, "/tpt_homog/indices/B"], 
	indsAvoid
	}, 
		indsAvoid = Intersection[Ainds, Binds];
		{Ainds, Binds, indsAvoid}
	]


(* ::Subsection:: *)
(*PolygonColors*)


PolygonColorsOpts={PlotExponent->1/4, PolygonOpacity->0.8, PolygonColorFunction->EurekaColorSmooth};
Options[PolygonColors]=Join[
					PolygonColorsOpts,
					Options[ABLegend]
					];

PolygonColors[fulam_, scalar_, opts:OptionsPattern[]]:=
	With[
	{PlotExponent = OptionValue[PlotExponent], PolygonOpacity = OptionValue[PolygonOpacity], PolygonColorFunction = OptionValue[PolygonColorFunction],
	AColor = OptionValue[AColor], BColor = OptionValue[BColor], DisconColor = OptionValue[DisconColor], AvoidColor = OptionValue[AvoidColor]}, 
	Module[{polys, polysDis, indsA, indsB, indsAvoid, maxScalar, polycolor, polycolorDis},
		{polys, polysDis} = ParseHDF5Polygons[fulam];
		{indsA, indsB, indsAvoid}=ParseABInds[ftpt];
		If[Length[scalar]!=Length[polys],Print["Scalar and polys have different dimensions."]; Abort[]];
		maxScalar=Max[scalar];
		polycolor = 
			Table[
				Which[
					MemberQ[indsAvoid, i],{FaceForm[AvoidColor], polys[[i]]},
					MemberQ[indsA, i],{FaceForm[AColor], EdgeForm[Directive[Thin, LightGray]], polys[[i]]},
					MemberQ[indsB, i],{FaceForm[BColor], EdgeForm[Directive[Thin, LightGray]], polys[[i]]},
					True, {Opacity[PolygonOpacity], FaceForm[PolygonColorFunction[(scalar[[i]] / maxScalar)^(PlotExponent)]], EdgeForm[Directive[Thin, LightGray]], polys[[i]]}
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


PlotScalarOpts={WorldRange->{{-100,-59},{6,32}}};
Options[PlotScalar]=Join[
					PlotScalarOpts,
					ScalarLegendOpts,
					Options[PolygonColors],
					Options[WorldTicks],
					Options[WorldPolygon]
					];

PlotScalar[fulam_, scalar_, opts:OptionsPattern[]] :=
	With[{WorldRange=OptionValue[WorldRange], PlotExponent = OptionValue[PlotExponent]}, 
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
					ImageSize -> 800], 
			ABLegend[InheritOpts[PlotScalar,ABLegend,opts]]
			], 
		ScalarLegend[scalar, InheritOpts[PlotScalar,ScalarLegend,opts]]
		]
	]
]


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


ftpt="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/TPT_stat_test.h5";
scalar=Import[ftpt,"/tpt_homog/statistics/normalized_reactive_density"];
fulam="/Users/gagebonner/Desktop/Repositories/TransitionPathTheory.jl/src/ulam_test.h5";
Options[PlotScalar]//Sort//MatrixForm


Graphics[ParseHDF5Polygons[fulam]]
s=PlotScalar[fulam, scalar,
	WorldRange->{{-100,15},{-9,39}},WorldTicksX->{-100, 0, 15}, WorldTicksY->{-9,0,39}, ScalarLegendLabel->"normalized_reactive_density", PlotExponent->1/6, ScalarLegendPlaced->{0.9,0.6}, ABPlaced->{0.077,0.22}, AvoidColor->None]


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


 vor={{100.0,   281.499},
 {121.0,   239.995},
 {142.0,   306.265},
 {163.0,   277.168},
 {184.0,  246.145},
 {205.0,   651.11},
 {226.0,   488.669},
 {247.0,   530.332},
 {268.0,   606.368},
 {289.0,   641.524},
 {310.0,  1157.43},
 {331.0,   687.108},
 {352.0,   576.756},
 {373.0,   920.653},
 {394.0,   843.99},
 {415.0,   875.188},
 {436.0,   832.674},
 {457.0,   986.424},
 {478.0,  1220.52},
 {500.0,  1453.72}};
 vor=Table[{vor[[i,1]],Log10[vor[[i,2]]*5/365]},{i,1,Length[vor]}]
 ListPlot[vor]


reg={ {142.0,   410.677},
 {158.0,   257.117},
 {180.0,   386.862},
 {205.0,   731.095},
 {215.0,   333.68},
 {236.0,   652.826},
 {255.0,   452.652},
 {279.0,   317.513},
 {288.0,  1836.43},
 {317.0,   310.305},
 {329.0,  2383.11},
 {357.0,  2643.47},
 {366.0,  2475.84},
 {399.0,   542.185},
 {410.0,   655.257},
 {419.0,  2052.68},
 {450.0,  1821.04},
 {465.0,   617.491},
 {491.0,  2492.59},
 {502.0,  1494.06}};
 reg=Table[{reg[[i,1]],Log10[reg[[i,2]]*5/365]},{i,1,Length[reg]}]
 ListPlot[reg]


ListPlot[{reg,vor}]
