(* ::Package:: *)

(* ::Section:: *)
(*EurekaColors*)


BeginPackage["EurekaColors`"]

EurekaColorDiscrete::usage =
"EurekaColorDiscrete[z] returns a color according to the Eureka color scheme for 0<=z<=1 binned into 16 bins."
        
EurekaColorSmooth::usage =
"EurekaColorSmooth[z] returns a color according to the Eureka color scheme, varying smoothly for 0<=z<=1."        


Begin["`Private`"]


eurcolors={
	RGBColor[{1, 1, 0.99609375}],
	RGBColor[{0.892708333333333, 0.945833333333333, 0.99609375}],
	RGBColor[{0.785416666666667, 0.891666666666667, 0.99609375}],
	RGBColor[{0.678125, 0.8375, 0.99609375}],
	RGBColor[{0.62421875, 0.81015625, 0.95625}],
	RGBColor[{0.73046875, 0.86328125, 0.796875}],
	RGBColor[{0.83671875, 0.91640625, 0.6375}],
	RGBColor[{0.94296875, 0.96953125, 0.478125}],
	RGBColor[{0.96953125, 0.91640625, 0.3453125}],
	RGBColor[{0.91640625, 0.75703125, 0.2390625}],
	RGBColor[{0.86328125, 0.59765625, 0.1328125}],
	RGBColor[{0.81015625, 0.43828125, 0.0265625000000001}],
	RGBColor[{0.7171875, 0.31875, 0}],RGBColor[{0.6109375, 0.2125, 0}],
	RGBColor[{0.5046875, 0.10625, 0}],RGBColor[{0.3984375, 0, 0}]
	};

EurekaColorDiscrete[z_?NumericQ]:=
	Module[{eur=eurcolors},
	Piecewise[
		Table[
			{eur[[i]], i/16 <= z <= (i+1)/16},
			{i,0,15}]
		]
	]

fRed[z_]:= 1.6948288689925386` -23.261258708411777` z+287.2526163432904` z^2-1810.4518732444687` z^3+5628.963095381033` z^4-7011.045446857542` z^5-4882.209365117081` z^6+26678.38600331812` z^7-34352.922053205824` z^8+20056.376018983447` z^9-4572.384221792814` z^10
fGreen[z_]:= 0.8030877975937706` +10.631598957452278` z-210.92580293897421` z^2+2051.2692616728787` z^3-11700.41291714229` z^4+40848.42630045395` z^5-88740.67351597158` z^6+119937.22476103417` z^7-97956.1258709181` z^8+44251.35374756834` z^9-8491.570696847932` z^10
fBlue[z_]:=Piecewise[{
			{1, 0<=z<=0.33},
			{1.6875` -2.08333 z, 0.33<z<=0.78},
			{0, 0.78<z<=1.0}
		}]

EurekaColorSmooth[z_?NumericQ]:=
	Module[{fR=fRed[z],fG=fGreen[z],fB=fBlue[z]},
		If[0<=z<=0.03, 
			White,
			RGBColor[{fR,fG,fB}]
		]
	]


End[]


EndPackage[]
