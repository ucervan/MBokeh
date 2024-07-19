(* Wolfram Language package *)

(* ::Section:: *)
(* Typesetting *)

$ListColor			= Orange;
$ClassColor 		= Brown;
$FigureClassColor 	= Blue;
$EventClassColor	= Magenta;


(* ::Subsection:: *)
(* Utility Classes *)

Clear[defaultListStyle]
SetAttribute[defaultListStyle, HoldFirst]
defaultListStyle[p_, name_String] := 
	With[{slen = ToString[p.len[]]},
		TooltipBox[StyleBox[name <> "[<"<>slen<>">]",
			FontColor -> $ListColor, FontSlant->Italic],
			MakeBoxes[slen, StandardForm]
		]
	]

$templateListTypesetting =
"
`a` /:	MakeBoxes[p : `a`[op_, ___] , StandardForm]	:= defaultListStyle[p, \"`b`\"]
"

ToExpression[StringTemplate[$templateListTypesetting][<|"a"->#[[1]], "b"->#[[2]]|>]]& /@ {
	{"BagClass"				, "Bag"},
	{"ListAttrSplatClass"	, "ListAttrSplat"}
}


(* ::Subsection:: *)
(* Bokeh Classes *)

Clear[defaultStyle]
SetAttribute[defaultStyle, HoldFirst]
defaultStyle[p_, slant_:Plain, color_:$ClassColor, weight_:Plain] :=
	With[{id = p.id},
		TooltipBox[StyleBox[If[!HasAttrQ[p, "subtype"], p.type, p.subtype] <> "[<\[CenterEllipsis]>]",
			FontColor -> color, FontSlant -> slant, FontWeight -> weight], 
			MakeBoxes[id, StandardForm]
		]
	]

$templateBokehTypesetting =
"
`a` /:	MakeBoxes[p : `a`[op_, ___] , StandardForm]	:= defaultStyle[p]
"

ToExpression[StringTemplate[$templateBokehTypesetting][<|"a"->#[[1]], "b"->#[[2]]|>]]& /@ 
	(
	{#, StringReplace[#, "Class" -> ""]} & /@ (ToString[#] & /@ 
   			Complement[MBokehClasses, 
   				{	
   					BokehAttributeClass, FigureClass
   				}
   				])
   )

BokehAttributeClass	/: 	MakeBoxes[p : BokehAttributeClass[op_, ___]	, StandardForm]	:= defaultStyle[p, Italic]
FigureClass 		/:	MakeBoxes[p : FigureClass[op_, ___]			, StandardForm]	:= defaultStyle[p, Plain, $FigureClassColor, Bold]


(* ::Subsection:: *)
(* Event Classes *)

Clear[defaultEventStyle]
SetAttribute[defaultEventStyle, HoldFirst]
defaultEventStyle[p_, name_, slant_:Italic, color_:$EventClassColor, weight_:Plain] :=
	With[{id = 1},
		TooltipBox[StyleBox[name <> "[<\[CenterEllipsis]>]",
			FontColor -> color, FontSlant -> slant, FontWeight -> weight], 
			MakeBoxes[id, StandardForm]
		]
	]

$templateBokehEventTypesetting =
"
`a` /:	MakeBoxes[p : `a`[op_, ___] , StandardForm]	:= defaultEventStyle[p, \"`b`\"]
"

ToExpression[StringTemplate[$templateBokehEventTypesetting][<|"a"->#[[1]], "b"->#[[2]]|>]]& /@ 
	(
	{#, StringReplace[#, "Class" -> ""]} & /@ (ToString[#] & /@ 
   			Complement[MBokehEventClasses, 
   				{	
   					
   				}
   				])
   )

