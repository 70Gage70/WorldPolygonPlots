(* ::Package:: *)

(* ::Section:: *)
(*ABLegend*)


BeginPackage["ABLegend`"]

ABLegend::usage = "ABLegend[opts] creates a swatch legend for A, B, disconnected and avoided polygons."


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


<<MaTeX`


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
								Graphics[{Opacity[1],EdgeForm[Directive[Thin,Black]],Rectangle[]}],
							{i,1,Length[colors]}]
				],
			ABPlaced
			]
		]
	]


End[]


EndPackage[]
