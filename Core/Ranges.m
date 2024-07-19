(* Wolfram Language package *)

(*
		, RangeClass
			, Range1dClass
			, DataRangeClass
				, DataRange1dClass
			, FactorRangeClass
*)


(* ::Section:: *)
(* RangeClass *)

RangeClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
			"callback"		-> "Instance"[CallbackClass]	(* Instance(Callback *)
		}
	]
	
RangeClass.init[opts : OptionsPattern[RangeClass] ] :=
	Module[{attrs},
		o.type = "Range";

		(* attribute fields *)
		attrs = o.attributes;
		attrs."callback" = Null;
	]


(* ::Subsection:: *)
(* Range1dClass *)

Range1dClass =
	NewClass[
		"Parents"		-> {RangeClass},
		"Fields"		-> {
			"start"				-> "Either"[0, {"BFloat", "Datetime", "BInt"}]
			, "end"				-> "Either"[1, {"BFloat", "Datetime", "BInt"}]
			, "bounds"			-> "MinMaxBounds"[None]			(* MinMaxBounds(accept_datetime=True, default=None *)
			, "min_interval"	-> "Either"[None, {"BFloat", "TimeDelta", "BInt"}]
			, "max_interval"	-> "Either"[None, {"BFloat", "TimeDelta", "BInt"}]
		}
	]
	
Range1dClass.init[opts : OptionsPattern[Range1dClass] ] :=
	Module[{attrs},
		o.type = "Range1d";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DataRangeClass *)

DataRangeClass =
	NewClass[
		"Parents"		-> {RangeClass},
		"Fields"		-> {
			"names"			-> "BList"[{}, {"BString"}]
			, "renderers"	-> Null (* List[BokehAttributeClass] BagClass *)
(*
    renderers = List(Instance(Renderer))
    An explicit list of renderers to autorange against. If unset,
    defaults to all renderers on a plot.
*)
		}
	]
	
DataRangeClass.init[opts : OptionsPattern[DataRangeClass] ] :=
	Module[{attrs},
		o.type = "DataRange";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* DataRange1dClass *)

DataRange1dClass =
	NewClass[
		"Parents"		-> {DataRangeClass},
		"Fields"		-> {
			"range_padding"			-> "BFloat"[0.1]
			, "range_padding_units"	-> "Enum"["percent", EnumPaddingUnits]
			, "start"				-> "BFloat"["auto"]
			, "end"					-> "BFloat"[0]
			, "bounds"				-> "MinMaxBounds"[None]
			, "min_interval"		-> "BFloat"[None]
			, "max_interval"		-> "BFloat"[None]
			, "flipped"				-> "BBool"[False]
			, "follow"				-> "Enum"[None, EnumStartEnd]
			, "follow_interval"		-> "BFloat"[None]
			, "default_span"		-> "BFloat"[2.0]
		}
	]
	
DataRange1dClass.init[opts : OptionsPattern[DataRange1dClass] ] :=
	Module[{attrs},
		o.type = "DataRange1d";
		
		(* attribute fields *)
		attrs = o.attributes;
		
	]


(* ::Subsection:: *)
(* FactorRangeClass *)

FactorRangeClass =
	NewClass[
		"Parents"		-> {RangeClass},
		"Fields"		-> {
			"_args"					-> {"factors"}
			, "factors"				-> "Either"[{}, {"BList"}]
			, "factor_padding"		-> "BFloat"[0.0]
			, "subgroup_padding"	-> "BFloat"[0.8]
			, "group_padding"		-> "BFloat"[1.4]
			, "range_padding"		-> "BFloat"[0]
			, "rage_padding_units"	-> "Enum"["percent", EnumPaddingUnits]
			, "start"				-> "BFloat"[Null]
			, "end"					-> "BFloat"[Null]
			, "bounds"				-> "MinMaxBounds"[None]
			, "min_interval"		-> "BFloat"[None]
			, "max_interval"		-> "BFloat"[None]
		}
	]
	
FactorRangeClass.init[opts : OptionsPattern[FactorRangeClass] ] :=
	Module[{attrs},
		o.type = "FactorRange";

		(* attribute fields *)
		attrs = o.attributes;
	]
