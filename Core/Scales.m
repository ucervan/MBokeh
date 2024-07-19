(* Wolfram Language package *)

(*
		, ScaleClass
			, LinearScaleClass
				, CategoricalScaleClass
			, LogScaleClass
*)


(* ::Section:: *)
(* ScaleClass *)

ScaleClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
		}
	]
	
ScaleClass.init[opts : OptionsPattern[ScaleClass] ] :=
	Module[{attrs},
		o.type = "Scale";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* LinearScaleClass *)

LinearScaleClass =
	NewClass[
		"Parents"		-> {ScaleClass},
		"Fields"		-> {
		}
	]
	
LinearScaleClass.init[opts : OptionsPattern[LinearScaleClass] ] :=
	Module[{attrs},
		o.type = "LinearScale";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsubsection:: *)
(* CategoricalScaleClass *)

CategoricalScaleClass =
	NewClass[
		"Parents"		-> {LinearScaleClass},
		"Fields"		-> {
		}
	]
	
CategoricalScaleClass.init[opts : OptionsPattern[CategoricalScaleClass] ] :=
	Module[{attrs},
		o.type = "CategoricalScale";

		(* attribute fields *)
		attrs = o.attributes;
	]
	
(* ::Subsection:: *)
(* LogScaleClass *)

LogScaleClass =
	NewClass[
		"Parents"		-> {ScaleClass},
		"Fields"		-> {
		}
	]
	
LogScaleClass.init[opts : OptionsPattern[LogScaleClass] ] :=
	Module[{attrs},
		o.type = "LogScale";

		(* attribute fields *)
		attrs = o.attributes;
	]
