(* ::Package:: *)

ConstructFrame[georange_,nx_,ny_,deltax_,deltay_,paddingsizex_,paddingsizey_,axesmag_,textmag_,angle_,slope_]:=Module[{xrng,yrng,bw,labelerx,labelery,boxes={},paddingboxes={},gridlines={},xlabels={},ylabels={},world},
xrng=Range[georange[[2,1]],georange[[2,2]],Ceiling[(georange[[2,2]]-georange[[2,1]])/nx]];
yrng=Range[georange[[1,1]],georange[[1,2]],Ceiling[(georange[[1,2]]-georange[[1,1]])/ny]];
If[xrng[[-1]]!=georange[[2,2]],AppendTo[xrng,georange[[2,2]]]];
If[yrng[[-1]]!=georange[[1,2]],AppendTo[yrng,georange[[1,2]]]];
bw[i_]:=If[OddQ[i],Black,White];
(*Constructing x boxes*);
Do[
AppendTo[boxes,
{GeoStyling[Opacity[1]],FaceForm[bw[i]],EdgeForm[Black],GeoBoundsRegion[{{georange[[1,1]],georange[[1,1]]+deltax},{xrng[[i]], xrng[[i+1]]}}]}
];
AppendTo[boxes,
{GeoStyling[Opacity[1]],FaceForm[bw[i]],EdgeForm[Black],GeoBoundsRegion[{{georange[[1,2]],georange[[1,2]]-deltax},{xrng[[i]], xrng[[i+1]]}}]}
],
{i,1,Length[xrng]-1}
];
(*Constructing y boxes*);
Do[
AppendTo[boxes,
{GeoStyling[Opacity[1]],FaceForm[bw[i]],EdgeForm[Black],GeoBoundsRegion[{{yrng[[i]], yrng[[i+1]]},{georange[[2,1]],georange[[2,1]]+deltay}}]}
];
AppendTo[boxes,
{GeoStyling[Opacity[1]],FaceForm[bw[i]],EdgeForm[Black],GeoBoundsRegion[{{yrng[[i]], yrng[[i+1]]},{georange[[2,2]]-deltay,georange[[2,2]]}}]}
],
{i,1,Length[yrng]-1}
];
(*Constructing x padding boxes*);
AppendTo[paddingboxes,
{GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[White],GeoBoundsRegion[{{georange[[1,1]]-0.07,georange[[1,1]]-paddingsizex-1},{-179.9,179.9}}]}
];
(*Constructing y padding boxes*);
AppendTo[paddingboxes,
{GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[Directive[Thick,White]],GeoBoundsRegion[{{-89.9,89.9},{georange[[2,1]]-paddingsizey-1,georange[[2,1]]-0.09}}]}
];
(*Constructing x gridlines*);
Do[AppendTo[gridlines,
{Opacity[0.2],Black,GeoPath[{{georange[[1,1]],xrng[[i]]},{georange[[1,2]],xrng[[i]]}}]}],{i,1,Length[xrng]}];
(*Constructing y gridlines*);
Do[AppendTo[gridlines,
{Opacity[0.2],Black,GeoPath[{{yrng[[i]],georange[[2,1]]},{yrng[[i]],georange[[2,2]]}},"Rhumb"]}],{i,1,Length[yrng]}];
(*Constructing x labels*);
labelerx[xval_]:=If[xval==0,MaTeX["0^\\circ",Magnification->textmag],If[xval<0,MaTeX[ToString[Abs[xval]]<>"^\\circ \\, \\text{W}",Magnification->textmag],MaTeX[ToString[xval]<>"^\\circ \\, \\text{E}",Magnification->textmag]]];
Do[AppendTo[xlabels,
{GeoMarker[{georange[[1,1]]-paddingsizex/4,xrng[[i]]}, Text[labelerx[xrng[[i]]]],"Alignment"->Center,"Scale"->axesmag]}],{i,2,Length[xrng]-1}];
(*Constructing y labels*);
labelery[yval_]:=If[yval==0,MaTeX["0^\\circ",Magnification->textmag],If[yval<0,MaTeX[ToString[Abs[yval]]<>"^\\circ \\, \\text{S}",Magnification->textmag],MaTeX[ToString[yval]<>"^\\circ \\, \\text{N}",Magnification->textmag]]];
Do[AppendTo[ylabels,
{GeoMarker[{yrng[[i]],georange[[2,1]]-paddingsizey/1.1-slope*(i-Length[yrng/2])},Rotate[ Text[labelery[yrng[[i]]]],angle],"Alignment"->Center,"Scale"->axesmag]}],{i,1,Length[yrng]-1}];
(*world shapes*);
world={GeoStyling[Opacity[1]],FaceForm[RGBColor[0.65,0.65,0.65]],EdgeForm[None],EntityValue[Entity["GeographicRegion","World"],EntityProperty["GeographicRegion","Polygon"]]};
Join[{world,boxes,gridlines,paddingboxes,xlabels,ylabels}]
]


dr=Reverse[{{-67, 0},{46, 66}}]
frame=ConstructFrame[dr,9,5,0.5,0.9,3,4,1,2,(\[Pi]/180)*(-16),0.15];
GeoGraphics[frame,
PlotLabel->MaTeX["test",Magnification->2.0],
GeoRange->dr-{{3,0},{4,0}},
GeoRangePadding->{None,None},
GeoBackground->None,
GeoGridLines->None,
GeoProjection->"Albers",
ImageSize->1100]


(* ::Section:: *)
(*DeepFlow*)


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<MaTeX`
<<EurekaColors`
<<WLHelpers`


(*constants*)


(* ::Subsection:: *)
(*FancyFrame*)


GBR[xmin_,xmax_,ymin_,ymax_]:=GeoBoundsRegion[{{ymin, ymax}, {xmin, xmax}}]

BlackWhiteBoxes[fbSize_,ticksX_,ticksY_]:=
	Module[{xmin, xmax, ymin, ymax, fbXB, fbXT, fbYL, fbYR, boxesBottom, boxesTop, boxesLeft, boxesRight, padTicksX},
		{xmin,xmax}=MinMax[ticksX];
		{ymin,ymax}=MinMax[ticksY];
		{{fbYL, fbYR}, {fbXB, fbXT}}=fbSize;
		padTicksX=ticksX;padTicksX[[1]]=padTicksX[[1]]-fbYL;padTicksX[[-1]]=padTicksX[[-1]]+fbYR;
		boxesBottom=Table[{GeoStyling[Opacity[1]],FaceForm[If[OddQ[i],Black,White]],EdgeForm[Black],GBR[padTicksX[[i]],padTicksX[[i+1]],ymin-fbXB,ymin]},{i,1,Length[padTicksX]-1}];
		boxesTop=Table[{GeoStyling[Opacity[1]],FaceForm[If[OddQ[i],Black,White]],EdgeForm[Black],GBR[padTicksX[[i]],padTicksX[[i+1]],ymax,ymax+fbXT]},{i,1,Length[padTicksX]-1}];
		boxesLeft=Table[{GeoStyling[Opacity[1]],FaceForm[If[OddQ[i],Black,White]],EdgeForm[Black],GBR[xmin-fbYL,xmin,ticksY[[i]],ticksY[[i+1]]]},{i,1,Length[ticksY]-1}];
		boxesRight=Table[{GeoStyling[Opacity[1]],FaceForm[If[OddQ[i],Black,White]],EdgeForm[Black],GBR[xmax,xmax+fbYR,ticksY[[i]],ticksY[[i+1]]]},{i,1,Length[ticksY]-1}];
		Join[boxesBottom,boxesTop,boxesLeft,boxesRight]
	]
	
	
TicksPaddingBoxes[ftSize_,xmin_,xmax_,ymin_,ymax_]:=
	Module[{ftX, ftY, xPad, yPad},
		{ftX, ftY}=ftSize;
		padTicksX=ticksX;padTicksX[[1]]=padTicksX[[1]]-fbYL;padTicksX[[-1]]=padTicksX[[-1]]+fbYR;
		boxesBottom={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[None],GBR[xmin,xmax,ymin-FancyTicksPadding[[2]],ymin]};
		boxesLeft=Table[{GeoStyling[Opacity[1]],FaceForm[If[OddQ[i],Black,White]],EdgeForm[Black],GBR[xmin-fbYL,xmin,ticksY[[i]],ticksY[[i+1]]]},{i,1,Length[ticksY]-1}];
		Join[boxesBottom,boxesLeft]
	]


FancyFrameOpts={FancyTicksX->{0, -3, -20, -30, -40, -67}, FancyTicksY->{46, 50, 53, 60, 61, 66}, FancyBoxesSize->{{3, 3},{3, 3}}, FancyTicksPadding->{3,3}};
Options[FancyFrame]=FancyFrameOpts;

FancyFrame[opts:OptionsPattern[]]:=
	With[{FancyTicksX = Sort[OptionValue[FancyTicksX]], FancyTicksY = Sort[OptionValue[FancyTicksY]], FancyBoxesSize = OptionValue[FancyBoxesSize], FancyTicksPadding = OptionValue[FancyTicksPadding]}, 
	Module[{xmin, xmax, ymin, ymax, bwboxes, newrange, xLabelsPad, xLabels},
		{xmin,xmax}=MinMax[FancyTicksX];
		{ymin,ymax}=MinMax[FancyTicksY];
		bwboxes=BlackWhiteBoxes[FancyBoxesSize,FancyTicksX,FancyTicksY];
		{{xmin,xmax},{ymin,ymax}}={{xmin,xmax},{ymin,ymax}}+FancyBoxesSize*{{-1,1},{-1,1}};
		xLabelsPad={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[None],GBR[xmin,xmax,ymin-FancyTicksPadding[[2]],ymin]};
		newrange={{xmin,xmax},{ymin,ymax}}-{{FancyTicksPadding[[1]],0},{FancyTicksPadding[[2]],0}};
		{Join[bwboxes,xLabelsPad],newrange}
		]
	]


GeoGraphics[{GeoStyling[Opacity[1]],FaceForm[Red],GBR[-67, 0, 43, 46]}]


gr=Reverse[{{-67, 0},{46, 66}}];
fbSize={{1,1},{1,1}};
worldtest={GeoStyling[Opacity[1]], FaceForm[Blue], EdgeForm[None], CountryData["World", "Polygon"]};
{frame,newrange} = FancyFrame[FancyBoxesSize->fbSize, FancyTicksPadding->{3,3}];
GeoGraphics[
	Join[worldtest,frame],
	PlotLabel->MaTeX["test",Magnification->2.0],
	GeoRange->Reverse[newrange],
	GeoRangePadding->{None,None},
	GeoBackground->None,
	GeoGridLines->None,
	GeoProjection->"Albers",
	ImageSize->1100
]


newrange
