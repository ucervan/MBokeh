(* Wolfram Language package *)

(*
				, GuideRendererClass
 					, AxisModelClass
						, ContinousAxisClass
							, LinearAxisClass
								, DatetimeAxisClass
								, MercatorAxisClass
							, LogAxisClass
						, CategoricalAxisClass
 *)


(* ::Section:: *)
(* AxisModelClass *)

AxisModelClass =
	NewClass[
		"Parents"		-> {GuideRendererClass},
		"Fields"		-> {
			"bounds" 					-> "auto" (* Either(Auto, Tuple(Float, Float), Tuple(Datetime, Datetime) *)
			, "x_range_name"			-> "BString"["default"] (* String('default', *)
			, "y_range_name"			-> "BString"["default"] (* String('default', *)
			, "ticker"					-> "Instance"[TickerClass] 	(* Instance(Ticker, help="""
    A Ticker to use for computing locations of axis components.

    The property may also be passed a sequence of floating point numbers as
    a shorthand for creating and configuring a ``FixedTicker``, e.g. the
    following code

    .. code-block:: python

        from bokeh.plotting import figure

        p = figure()
        p.xaxis.ticker = [10, 20, 37.4]

    is equivalent to:

    .. code-block:: python

        from bokeh.plotting import figure
        from bokeh.models.tickers import FixedTicker

        p = figure()
        p.xaxis.ticker = FixedTicker(ticks=[10, 20, 37.4])

    """).accepts(Seq(Float), lambda ticks: FixedTicker(ticks=ticks)) *)

		, "formatter"					-> "Instance"[TickFormatterClass]	(* Instance(TickFormatter, *)
		, "axis_label"					-> "BString"[""] 					(* String(default='', *) 
		, "axis_label_standoff"			-> "BInt"[5]						(* Int(default=5, *)
		, IncludeProps[TextProps, "axis_label",
			"axis_label_text_font_size"		-> "FontSizeSpec"["10pt"] 		(* Override(default={'value': "10pt"}) *)
			, "axis_label_text_font_style"	->  "italic"					(* Enum(FontStyle *)
		]
		, "major_label_standoff"		-> "BInt"[5] 						(* Int(default=5, *)
		, "major_label_orientation"		-> "horizontal"						(* Either(Enum("horizontal", "vertical"), Float, *)
		, "major_label_overrides"		-> "Dic"[<||>] 						(* Dict(Either(Float, String), String, default={}, *)
		, IncludeProps[TextProps, "major_label",
			"major_label_text_align"		-> "center" 					(* Enum(TextAlign) Override(default="center") *)
			, "major_label_text_baseline"	-> "alphabetic" 				(* Enum(TextBaseline) Override(default="alphabetic") *)
			, "major_label_text_font_size"	-> "FontSizeSpec"["8pt"]		(* Override(default={'value': "8pt"} *)
		]
		, IncludeProps[LineProps, "axis"]
		, IncludeProps[LineProps, "major_tick"]
		, "major_tick_in"				-> "BInt"[2] 						(* Int(default=2 *)
		, "major_tick_out"				-> "BInt"[6]						(* Int(default=6 *)
		, IncludeProps[LineProps, "minor_tick"]
		, "minor_tick_in"				-> "BInt"[0]						(* Int(default=0, *)
		, "minor_tick_out"				-> "BInt"[4]						(* Int(default=4, *)
		, "fixed_location"				-> "Either"[None, {"BFloat", "BString", "BList"}]
		}
	]
	
AxisModelClass.init[opts : OptionsPattern[AxisModelClass] ] :=
	Module[{attrs},
		o.type = "Axis";
		
		If[MatchQ[o.ticker, MBokehClassSymbols],
			o.ticker = New[o.ticker][]
		];

		If[MatchQ[o.formatter, MBokehClassSymbols],
			o.formatter = New[o.formatter][]
		];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ContinousAxisClass *)

ContinousAxisClass =
	NewClass[
		"Parents"		-> {AxisModelClass},
		"Fields"		-> {
		}
	]
	
ContinousAxisClass.init[opts : OptionsPattern[ContinousAxisClass] ] :=
	Module[{attrs},
		o.type = "ContinousAxis";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* LinearAxisClass *)

LinearAxisClass =
	NewClass[
		"Parents"		-> {ContinousAxisClass},
		"Fields"		-> {
			"ticker"		-> "Instance"[TickerClass]
			, "formatter"	-> "Instance"[TickFormatterClass]
		}
	]
	
LinearAxisClass.init[opts : OptionsPattern[LinearAxisClass] ] :=
	Module[{attrs},
		o.type = "LinearAxis";

		o."ticker" 		= BasicTicker[];
		o."formatter" 	= BasicTickFormatter[];

(*

		If[!IsInstanceQ[o."ticker", TickerClass],
			Print[o."ticker"];
			o."ticker" = BasicTicker[]
			];

		If[!IsInstanceQ[o."formatter", TickerFormatterClass],
			o."formatter" = BasicTickFormatter[]
			];
*)
		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* DatetimeAxisClass *)

DatetimeAxisClass =
	NewClass[
		"Parents"		-> {LinearAxisClass},
		"Fields"		-> {
			"ticker"		-> "Instance"[DatetimeTickerClass]
			, "formatter"	-> "Instance"[DatetimeTickFormatterClass]
		}
	]
	
DatetimeAxisClass.init[opts : OptionsPattern[DatetimeAxisClass] ] :=
	Module[{attrs},
		o.type = "DatetimeAxis";

		o."ticker" 		= DatetimeTicker[];
		o."formatter" 	= DatetimeTickFormatter[];

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* MercatorAxisClass *)

MercatorAxisClass =
	NewClass[
		"Parents"		-> {LinearAxisClass},
		"Fields"		-> {
			"ticker"		-> "Instance"[MercatorTickerClass]	
			, "formatter"	-> "Instance"[MercatorTickFormatterClass]
		}
	]
MercatorAxisClass.init[opts_] :=
	Module[{attrs},
		o.type = "MercatorAxis";
		
		If[o."ticker" === MercatorTickerClass,
			o."ticker" = MercatorTicker[]
		];
		
		If[o."formatter" === MercatorTickFormatterClass,
			o."formatter" = MercatorTickFormatter[]
		];
		
		(* attribute fields *)
		attrs = o.attributes;
	]
MercatorAxisClass.initInstance[opts : OptionsPattern[] ] :=
	Module[{dim},
		(*
		o.super.initInstance[opts];
		*)
		dim = "dimension" /. {opts};
		If[dim==="dimension", Return[]];
		If[IsInstanceQ[o."ticker", MercatorTickerClass], 
			o."ticker"."dimension" = dim
		];
		If[IsInstanceQ[o."formatter", MercatorTickFormatterClass], 
			o."formatter"."dimension" = dim
		];
	]

	
(* ::Subsection:: *)
(* LogAxisClass *)

LogAxisClass =
	NewClass[
		"Parents"		-> {ContinousAxisClass},
		"Fields"		-> {
			"ticker"		-> "Instance"[LogTickerClass]
			, "formatter"	-> "Instance"[LogTickFormatterClass]
		}
	]
	
LogAxisClass.init[opts : OptionsPattern[LogAxisClass] ] :=
	Module[{attrs},
		o.type = "LogAxis";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* CategoricalAxisClass *)

CategoricalAxisClass =
	NewClass[
		"Parents"		-> {AxisModelClass},
		"Fields"		-> {
			"ticker"						-> "Instance"[CategoricalTickerClass]
			, "formatter"					-> "Instance"[CategoricalTickFormatterClass]			
			, IncludeProps[LineProps, "separator",
				"separator_line_color"			-> "ColorSpec"["lightgrey"] 	(* Override(default="lightgrey") *)
				, "separator_line_width"		-> "NumberSpec"[2]				(* Override(default=2) *)
			]
			, IncludeProps[TextProps, "group",
				"group_text_font_size"			-> "FontSizeSpec"["8pt"]		(* Override(default={'value': "8pt"}) *)
				, "group_text_font_style"		-> "bold"						(* Enum(FontStyle) Override(default="bold") *)
				, "group_text_color"			-> "ColorSpec"["grey"]			(* Override(default="grey") *)
			]
			, IncludeProps[TextProps, "subgroup",
				"subgroup_text_font_size"		-> "FontSizeSpec"["8pt"]		(* Override(default={'value': "8pt"}) *)
				, "subgroup_text_font_style"	-> "bold"						(* Enum(FontStyle) Override(default="bold") *)
			]
		}
	]
	
CategoricalAxisClass.init[opts : OptionsPattern[CategoricalAxisClass] ] :=
	Module[{attrs},
		o.type = "CategoricalAxis";

		(* attribute fields *)
		attrs = o.attributes;
	]

