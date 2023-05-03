(* ::Package:: *)

(* ::Section:: *)
(*FancyGeoFrame*)


BeginPackage["WLHelpers`"]

FancyGeoFrame::usage = 
"InheritOpts[funcOuter,funcOpts,inputOpts] is a helper function for defining functions which need to take their own options as well as the options for some other function.
					
funcOuter is the current function you're defining.
funcOpts is the function whose options you want to make passable to the current function.
inputOpts is the OptionsPattern from the current function definition (i.e. that the user will pass or not pass.)

The result of InheritOpts should be passed as options to instances of funcOpts in funcOuter.
"

NumberToTeXString::usage = 
"NumberToTeXString[num, prec] returns a string in LaTeX form suitable for use with MaTeX.

num is the number
prec is the number of digits of precision you want in the number (default 3)
"
							
FractionToInlineLaTeX::usage = 
"FractionToLaTeX[num] converts a/b to a flat fraction (i.e. not \\frac) suitable for use with MaTeX.

num is the (rational) number
"


(* ::Subsection:: *)
(*Dependencies*)


SetDirectory[NotebookDirectory[]];

<<MaTeX`
<<EurekaColors`
<<WLHelpers`


(*constants*)


(* ::Subsection:: *)
(*FancyFrame*)


GeoTickOpts={GeoTickLatLon->"Arb", GeoTickMag->1.8};
Options[GeoTick]=GeoTickOpts;

GeoTick[coord_,opts:OptionsPattern[]]:=	
	With[{GeoTickLatLon=OptionValue[GeoTickLatLon], GeoTickMag=OptionValue[GeoTickMag]},
	Module[{degree},
		degree = Which[
			GeoTickLatLon == "Lon", Which[coord<0, "^\\circ \\text{W}", coord>0, "^\\circ \\text{E}", coord==0, "^\\circ"],
			GeoTickLatLon == "Lat", Which[coord<0, "^\\circ \\text{S}", coord>0, "^\\circ \\text{N}", coord==0, "^\\circ"],
			GeoTickLatLon == "Arb", "^\\circ"
			];
		MaTeX[ToString[Abs[coord]]<>degree,Magnification->GeoTickMag]
		]
	]


GBR[xmin_,xmax_,ymin_,ymax_]:=GeoBoundsRegion[{{ymin, ymax}, {xmin, xmax}}]


FrancyGridLines[op_,ticksX_,ticksY_]:=
	Module[{xmin, xmax, ymin, ymax, xgridlines, ygridlines},
		{xmin,xmax}=MinMax[ticksX];
		{ymin,ymax}=MinMax[ticksY];
		xgridlines=Table[{Opacity[op],Black,GeoPath[{{ymin,ticksX[[i]]},{ymax,ticksX[[i]]}},"Rhumb"]},{i,1,Length[ticksX]}];
		ygridlines=Table[{Opacity[op],Black,GeoPath[{{ticksY[[i]],xmin},{ticksY[[i]],xmax}},"Rhumb"]},{i,1,Length[ticksY]}];
		Join[xgridlines,ygridlines]
	]


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
	Module[{ftX, ftY, xPadBot, xPadTop, yPadLeft, yPadRight},
		{ftY, ftX}=ftSize;
		xPadBot={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[White],GBR[xmin-ftX,xmax,ymin-ftY,ymin]};
		xPadTop={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[White],GBR[xmin-ftX,xmax+ftX/2,ymax,ymax+ftY/2]};
		yPadLeft={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[White],GBR[xmin-ftX,xmin,ymin,ymax]};
		yPadRight={GeoStyling[Opacity[1]],FaceForm[White],EdgeForm[White],GBR[xmax,xmax+ftX/2,ymin-ftY,ymax]};
		Join[xPadBot,xPadTop,yPadLeft,yPadRight]
	]


FancyGeoFrameOpts={FancyBoxesSize->{{3, 3},{3, 3}}, FancyTicksPadding->{3,3}, FancyTicksMag->1.8, FancyGridlinesOpacity->0.2, FancyTicksAngle->{0,0}, FancyTicksYDelta->0};
Options[FancyGeoFrame]=FancyGeoFrameOpts;

FancyGeoFrame[FancyTicksX_, FancyTicksY_, opts:OptionsPattern[]]:=
	With[{
		FancyBoxesSize = OptionValue[FancyBoxesSize], 
		FancyTicksPadding = OptionValue[FancyTicksPadding], 
		FancyTicksMag = OptionValue[FancyTicksMag], 
		FancyGridlinesOpacity = OptionValue[FancyGridlinesOpacity], 
		FancyTicksAngle = OptionValue[FancyTicksAngle],
		FancyTicksYDelta=OptionValue[FancyTicksYDelta]}, 
	Module[{xmin, xmax, ymin, ymax, TicksX, TicksY, gridlines, bwboxes, ticksboxes, newrange, xLabels, yLabels, ftXAngle, ftYAngle, xCorrection, yCorrection},
		TicksX=Sort[FancyTicksX];
		TicksY=Sort[FancyTicksY];
		{xmin,xmax}=MinMax[TicksX];
		{ymin,ymax}=MinMax[TicksY];
		{ftXAngle,ftYAngle}=FancyTicksAngle;
		xCorrection=Table[(-((2i)/(xmax-xmin))+(xmax+xmin)/(xmax-xmin))ftXAngle,{i,TicksX}];
		yCorrection=Table[(i/(ymin-ymax)+ymax/(ymax-ymin))FancyTicksYDelta,{i,TicksY}];
		gridlines=FrancyGridLines[FancyGridlinesOpacity,TicksX,TicksY];
		bwboxes=BlackWhiteBoxes[FancyBoxesSize,TicksX,TicksY];
		{{xmin,xmax},{ymin,ymax}}={{xmin,xmax},{ymin,ymax}}+FancyBoxesSize*{{-1,1},{-1,1}};
		ticksboxes=TicksPaddingBoxes[FancyTicksPadding,xmin,xmax,ymin,ymax];
		newrange={{xmin,xmax},{ymin,ymax}}+{{-FancyTicksPadding[[2]],FancyTicksPadding[[2]]/2},{-FancyTicksPadding[[1]],FancyTicksPadding[[1]]/2}};
		xLabels=Table[GeoMarker[{ymin-FancyTicksPadding[[1]]/2,TicksX[[i]]},
			Rotate[Text[GeoTick[TicksX[[i]],GeoTickLatLon->"Lon",GeoTickMag->FancyTicksMag]],xCorrection[[i]]],
			"Alignment"->Center,"Scale"->Scaled[1]],{i,1,Length[TicksX]}];
		yLabels=Table[GeoMarker[{TicksY[[i]],xmin-FancyTicksPadding[[2]]/2+yCorrection[[i]]},
		Rotate[Text[GeoTick[TicksY[[i]],GeoTickLatLon->"Lat",GeoTickMag->FancyTicksMag]],ftYAngle],
			"Alignment"->Center,"Scale"->Scaled[1]],{i,1,Length[TicksY]}];
		{Join[gridlines, ticksboxes, bwboxes, xLabels, yLabels],newrange}
		]
	]


ticksX={0, -10, -20, -30, -40, -67};
ticksY={46, 50, 53, 57, 61, 66};
gr=Reverse[{{-67, 0},{46, 66}}];
fbSize={{1,1},{0.75,0.75}};
worldtest={GeoStyling[Opacity[1]], FaceForm[Gray], EdgeForm[None], CountryData["World", "Polygon"]};
{frame,newrange} = FancyGeoFrame[ticksX, ticksY, FancyBoxesSize->fbSize, FancyTicksPadding->{2,7.5},FancyTicksMag->2.5, FancyTicksAngle->{-13,-12}*\[Pi]/180,FancyTicksYDelta->1];
GeoGraphics[
	Join[worldtest,frame],
	GeoRange->Reverse[newrange],
	GeoRangePadding->{None,None},
	GeoBackground->None,
	GeoGridLines->None,
	GeoProjection->"Albers",
	ImageSize->1100
]
