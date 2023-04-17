(* ::Package:: *)

(* ::Section:: *)
(*WLHelpers*)


BeginPackage["WLHelpers`"]

InheritOpts::usage = 
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


Begin["`Private`"]


InheritOpts[funcOuter_,funcOpts_,inputOpts:OptionsPattern[]]:=
	Module[{replaceable,replacewith,rulename,rule},
		replaceable=FilterRules[Options[funcOuter],Options[funcOpts]];
		replacewith=Evaluate@FilterRules[{inputOpts}, Options[funcOpts]];
		Do[
			rule=replacewith[[i]];
			replaceable=replaceable/.(rule[[1]]->_)->rule
		,{i,1,Length[replacewith]}
		];
		replaceable
	]


NumberToTeXString[num_, prec_:3]:=
	Module[{me,man,res},
		me=MantissaExponent[N[num]];
		man=StringTake[
				ToString[
					NumberForm[me[[1]]*10^(prec+1),prec]
					],
			prec];
		res=Which[
		(-1<=me[[2]]<=0)||(1<=me[[2]]<=3),ToString[DecimalForm[SetPrecision[N[num], prec]]],
		(me[[2]]<-1)||(me[[2]]>3),StringTake[man,1]<>"."<>StringTake[man,{2;;prec}]<>"\\times 10^{"<>ToString[me[[2]]-1]<>"}"
		];
		If[StringTake[res,-1]==".",
			res=StringTake[res,{1;;-2}][[1]],
			res
		];
		res
	]//Quiet


FractionToInlineLaTeX[num_]:=
	If[Denominator[num]==1,ToString[Numerator[num]],
	ToString[Numerator[num]]<>"/"<>ToString[Denominator[num]]
	]


End[]


EndPackage[]


(*
ABLegendOpts={AColor->Hue[0.83,0.68,1.0], BColor->Red, DisconColor->Black, AvoidColor->LightGray, ABPlaced->{0.077,0.18}};
Options[ABLegend]=Join[
					ABLegendOpts,
					{Magnification->1.8}
					];

ABLegend[opts:OptionsPattern[]]:=
	With[{AColor = OptionValue[AColor], BColor = OptionValue[BColor], DisconColor = OptionValue[DisconColor], AvoidColor = OptionValue[AvoidColor], ABPlaced = OptionValue[ABPlaced]}, 
	Module[{colors,labels,valid,legs,optsMaTeXeval},
		colors={AColor,BColor,DisconColor,AvoidColor};
		optsMaTeXeval=InheritOpts[ABLegend,MaTeX,opts];
		labels={MaTeX["\\mathbb{A}",optsMaTeXeval],
				MaTeX["\\mathbb{B}",optsMaTeXeval],
				MaTeX["\\text{Discon.}",optsMaTeXeval],
				MaTeX["\\text{Avoid}",optsMaTeXeval]};
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
*)
