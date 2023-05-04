(* ::Package:: *)

(* ::Section:: *)
(*GeoTick*)


BeginPackage["GeoTick`"]

GeoTick::usage = "GeoTick[coordinate] returns a TeX'd version of coordinate."

WorldTicks::usage = "WorldTicks[opts] returns a TeX'd list of ticks suitable for use with FrameTicks."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`


(* ::Subsection:: *)
(*GeoTick*)


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


(* ::Subsection:: *)
(*WorldTicks*)


WorldTicksOpts={WorldTicksX->{-100,-80,-59}, WorldTicksY->{6,20,32}, WorldTicksMag->1.8};
Options[WorldTicks]=WorldTicksOpts;

WorldTicks[opts:OptionsPattern[]]:=
	With[{WorldTicksX=OptionValue[WorldTicksX], WorldTicksY=OptionValue[WorldTicksY], WorldTicksMag=OptionValue[WorldTicksMag]},
	Module[{x,y},
		x=Table[{xt, GeoTick[xt, GeoTickLatLon->"Lon", GeoTickMag->WorldTicksMag], {0,0.01}},
			{xt,WorldTicksX}
		];
		y=Table[{yt, GeoTick[yt, GeoTickLatLon->"Lat", GeoTickMag->WorldTicksMag], {0,0.01}},
			{yt,WorldTicksY}
		];
		{{y,None},{x,None}}
		]
	]


End[]


EndPackage[]
