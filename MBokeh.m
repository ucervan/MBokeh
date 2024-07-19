(* Mathematica Package *)

(* Copyright 2016-2018 by 1010data, Inc. Written by Ulises Cervantes-Pimentel *)

Global`PackageTimingStart[]

BeginPackage["MBokeh`", {"TypeSystem`", "Dataset`", "TenTenFormat`"}]
(* Exported symbols added here with SymbolName::usage *) 

$MBokehGlobalSymbols = {
	MBokeh`$RegisterClasses
	, MBokeh`$GetSafeClasses
	, MBokeh`$GetUnSafeClasses
	, MBokeh`$ProtectClasses
	, MBokeh`ValueWarning
	, MBokeh`ValueError
	, MBokeh`RuntimeError
}

$RegisterClasses::usage		= "$RegisterClasses[classes]"
$GetSafeClasses::usage		= "$GetSafeClasses[symblist]"
$GetUnSafeClasses::usage	= "$GetUnSafeClasses[symblist]"
$ProtectClasses::usage		= "$ProtectClasses[classes]"
ValueWarning::usage			= "ValueWarning"
ValueError::usage			= "ValueError"
RuntimeError::usage			= "RuntimeError"

(Unprotect[#]; ClearAll[#])& /@ $MBokehGlobalSymbols

Begin["`Private`"]

(* Implementation of the package *)

(* ::Section:: *)
(* Messages *)


ValueWarning[msg_String, vals_List] := Print[StringTemplate[msg][Sequence@@(ToString[#]& /@ vals)]]
ValueWarning[msg_String, val_] 		:= ValueWarning[msg, {val}]
ValueWarning[msg_String]			:= ValueWarning[msg, {}]

ValueError[msg_String, vals_List] 	:= Print[StringTemplate[msg][Sequence@@(ToString[#]& /@ vals)]]
ValueError[msg_String, val_] 		:= ValueError[msg, {val}]
ValueError[msg_String]				:= ValueError[msg, {}]

RuntimeError[msg_String, val_] 		:= ValueError[msg, val]
RuntimeError[msg_String] 			:= RuntimeError[msg, {}]

(* ::Section:: *)
(* Classes *)

$template =
"
`a`[opts : OptionsPattern[] ] := 
	Module[{obj},
		obj = New[`b`][opts];
		If[obj.$FailedInit, Return[$Failed]];
		If[IsInstanceQ[obj, BokehAttributeClass] && MemberQ[obj.getAllFunctions[], \"initInstance\"],
			obj.initInstance[opts]
		];
		obj
	]

`a`[vargs__, oopts : OptionsPattern[] ]/; ListQ[\"_args\"/. Options[`b`]] && Length[{vargs}] == Length[\"_args\"/. Options[`b`]]:= 
	Module[{largs, opts, res},
		largs =\"_args\" /. Options[`b`];
		opts = Join[MapThread[Rule,{largs,{vargs}}], {oopts}];
		 `a`[Sequence@@opts]
	]

"

$RegisterClasses[classes_List] /; Length[classes]>0 := MapThread[
  ToExpression[StringTemplate[$template][<|"a" -> #1, "b" -> #2|>]] &, 
  Transpose[{#, # <> "Class" } & /@ 
    Sort[ToString[#] & /@ classes]]]


$GetSafeClasses[classes_List] := If[(
           Context[#] =!= "System`" && DownValues[#] === {}), #, 
          Sequence @@ {}] & /@ (ToExpression[
            StringReplace[ToString[#], "Class" -> ""]] & /@ 
          classes)

$GetUnSafeClasses[classes_List] := If[(
           Context[#] === "System`" || DownValues[#] =!= {}), #, 
          Sequence @@ {}] & /@ (ToExpression[
            StringReplace[ToString[#], "Class" -> ""]] & /@ 
          classes)
          
$templateEnd =
"
If[Options[`a`] === {},
	Options[`a`] = Options[`b`]
]
SetAttributes[{`a`}, {ReadProtected, Protected}]
"

$ProtectClasses[classes_List] /; Length[classes]>0 := MapThread[
  ToExpression[StringTemplate[$templateEnd][<|"a" -> #1, "b" -> #2|>]] &, 
  Transpose[{#, # <> "Class" } & /@ 
    Sort[ToString[#] & /@ classes]]];

(* ::Section:: *)
(* TenTenKLink Interface *)
(* declare symbols from different context *)

(* K Environment *)
$KEnvironmentQ 	:= $KEnvironmentQ 	= (TenTenData`TenTenAPI`Private`$KEnvironmentQ===True)

KConsole 		= TenTenKLink`KConsole
KFail			= TenTenKLink`KFail
KAbort			= TenTenKLink`KAbort

End[]

SetAttributes[{#}, {ReadProtected, Protected}]& /@ $MBokehGlobalSymbols
Remove[$MBokehGlobalSymbols]

EndPackage[]

Scan[Get,
	{ 
	  "MBokeh`Core`MBokeh`"
	, "MBokeh`Core`MBokehClasses`"
	}
]

Global`PackageTimingEnd[]
