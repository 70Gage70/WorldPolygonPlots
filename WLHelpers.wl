(* ::Package:: *)

(* ::Section:: *)
(*WLHelpers*)


BeginPackage["WLHelpers`"]

DelegateOptions::usage = "DelegateOptions[spec1, spec2, ...] returns a sequence of options, extracted from given spec[i], filtered for head surrounding delegateOptions expression. \
Option specification speci can be explicit opt -> val rule or delayed rule, a symbol from which default options will be extracted, or a list of valid option specifications."

NumberToTeXString::usage = 
"NumberToTeXString[num, prec] returns a string in LaTeX form suitable for use with MaTeX.

num is the number
prec is the number of digits of precision you want in the number (default 3)
"
							
FractionToInlineLaTeX::usage = 
"FractionToLaTeX[num] converts a/b to a flat fraction (i.e. not \\frac) suitable for use with MaTeX.
"


Begin["`Private`"]


(* ::Subsection:: *)
(*Dependencies*)


(*None*)


(* ::Subsection:: *)
(*InheritOpts*)


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


(* ::Subsection:: *)
(*NumberToTeXString*)


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


(* ::Subsection:: *)
(*FractionToInlineLaTeX*)


FractionToInlineLaTeX[num_]:=
	If[Denominator[num]==1,ToString[Numerator[num]],
	ToString[Numerator[num]]<>"/"<>ToString[Denominator[num]]
	]


(* ::Subsection:: *)
(*DelegateOptions*)


DelegateOptions /: head_[args1___, DelegateOptions[opts___], args2___] :=
    head[
        args1,
        Sequence @@ FilterRules[
            Replace[Flatten[{opts}], sym_Symbol :> Options[sym], {1}], 
            Options[head]
        ],
        args2
    ]


End[]


EndPackage[]
