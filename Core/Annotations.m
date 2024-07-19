(* Wolfram Language package *)

(*
	Model
		, LegendItemClass
	RendererClass
		, AnnotationModelClass
			, TextAnnotationClass
				, **LabelSetClass
			, LegendClass
			, ColorBarClass
			, **ArrowClass
			, BoxAnnotationClass
			, **BandClass
			, LabelModelClass
			, PolyAnnotationClass
			, **SpanClass
			, TitleClass
			, TooltipModelClass
			, WhiskerClass
			, **ToolbarPanel
*)

(* ::Section:: *)
(* LegendItemClass *)

LegendItemClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
			"label"		-> "StringSpec"[None] 
(*	
    A label for this legend. Can be a string, or a column of a
    ColumnDataSource. If ``label`` is a field, then it must
    be in the renderers' data_source.
*)
			, "renderers" 	-> List[BokehAttributeClass] (* List(Instance(GlyphRenderer), *)
(*
    A list of the glyph renderers to draw in the legend. If ``label`` is a field,
    then all data_sources of renderers must be the same.
*)			
		}
	]
	
LegendItemClass.init[opts : OptionsPattern[LegendItemClass] ] :=
	Module[{attrs, bag},
		o.type = "LegendItem";

		If[StringQ[o.label], o.label = value[o.label]];
		Which[
			ListQ[o.renderers],
				bag = Bag["content"->(o.renderers)];
				o.renderers = bag,
			Head[o.renderers]===BagClass,
				None,
			True,
				bag = Bag["content"->o.renderers];
				o.renderers = bag
		];
		
		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* AnnotationModelClass *)

AnnotationModelClass =
	NewClass[
		"Parents"		-> {RendererClass},
		"Fields"		-> {
			"plot"		-> "Instance"[PlotModelClass]
			, "level"	-> "annotation" (* override *)
		}
	]
	
AnnotationModelClass.init[opts : OptionsPattern[AnnotationModelClass] ] :=
	Module[{attrs},
		o.type = "Annotation";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* TextAnnotationClass *)

(* abstract *)
TextAnnotationClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
		}
	]
	
TextAnnotationClass.init[opts : OptionsPattern[TextAnnotationClass] ] :=
	Module[{attrs},
		o.type = "TextAnnotation";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* LegendClass *)

LegendClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
			"location"					-> "top_right"
			, "orientation"				-> "vertical"
			, IncludeProps[LineProps, "border",
				"border_line_color"			-> "ColorSpec"["#e5e5e5"] 	(* override *)
				, "border_line_alpha"		-> "NumberSpec"[0.5]		(* override *)
			]
			, IncludeProps[FillProps, "background",
				"background_fill_color"		-> "ColorSpec"["#ffffff"] 	(* override *)
				, "background_fill_alpha"	-> "NumberSpec"[0.95]		(* override *)
			]
			, IncludeProps[FillProps, "inactive",
				"inactive_fill_color"		-> "ColorSpec"["white"]		(* override *)
				, "inactive_fill_alpha"		-> "NumberSpec"[0.9]		(* override *)
			]
			, "click_policy"			-> "none"
			, IncludeProps[TextProps, "label", 
				"label_text_baseline"		->"middle"		(* override *)
				, "label_text_font_size"	-> "FontSizeSpec"["10pt"]
				, "label_standoff"			-> "BInt"[5]
				, "label_height"			-> "BInt"[20]
				, "label_width"				-> "BInt"[20]
			]
			, "glyph_height"			-> "BInt"[20]
			, "glyph_width"				-> "BInt"[20]
			, "margin"					-> "BInt"[10]
			, "padding"					-> "BInt"[10]
			, "spacing"					-> "BInt"[3]
			, "items"					-> List[BokehAttributeClass]	(* BagClass *)
		}
	]
	
LegendClass.init[opts : OptionsPattern[LegendClass] ] :=
	Module[{attrs, itemslist, bag},
		o.type = "Legend";
		
		If[MatrixQ[(o."items")],
			itemslist = LegendItem["label"->#[[1]], "renderers" ->#[[2]] ]& /@ (o.items);
			bag = Bag["content"->itemslist];
			o.items = bag
		];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ColorBarClass *)

ColorBarClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
			"location"						-> "top_right"
			, "orientation"					-> "vertical"
			, "height"						-> "auto"
			, "width"						-> "auto"
			, "scale_alpha"					-> "BFloat"[1.0]
			, "title"						-> "BString"[""]
			, IncludeProps[TextProps, "title",
				"title_text_font_size"			-> "FontSizeSpec"["10pt"]
				, "title_text_font_style"		-> "italic" 							(* override *)
				, "title_standoff"				-> "BInt"[2]
			]
			, "ticker"						-> "Instance"[BasicTickerClass] 			(* Instance(Ticker, default=lambda: BasicTicker() *)
			, "formatter"					-> "Instance"[BasicTickFormatter]			(* Instance(TickFormatter, default=lambda: BasicTickFormatter() *)
			, "major_label_overrides"		-> <||>
			, "color_mapper"				-> "Instance"[ContinousColorMapperClass]	(* Instance(ContinuousColorMapper) *)
			, "margin"						-> "BInt"[30]
			, "padding"						-> "BInt"[10]
			, IncludeProps[TextProps, "major", 
				"major_label_text_align"			-> "center"				(* override *)
				, "major_label_text_baseline"		-> "middle"				(* override *)
				, "major_label_text_font_size"		-> "FontSizeSpec"["8pt"]
			]
			, "label_standoff"				-> "BInt"[5]
			, IncludeProps[LineProps, "major",
				"major_tick_line_color"		-> "ColorSpec"["#ffffff"]		(* override *)
				, "major_tick_in"			-> "BInt"[5]
				, "major_tick_out"			-> "BInt"[0]
			]
			, IncludeProps[LineProps, "minor",
				"minor_tick_line_color"		-> "ColorSpec"[None]			(* override *)
				, "minor_tick_in"				-> 0
				, "minor_tick_out"				-> 0
			]
			, IncludeProps[LineProps, "bar",
				"bar_line_color"				-> "ColorSpec"[None]		(* override *)
			]
			, IncludeProps[LineProps, "border",
				"border_line_color"				-> "ColorSpec"[None]		(* override *)
			]
			, IncludeProps[FillProps, "background",
				"background_fill_color"			-> "ColorSpec"["#ffffff"]	(* override *)
				, "background_fill_alpha"		-> "NumberSpec"[0.95]		(* override *)
			]
		}
	]
	
ColorBarClass.init[opts : OptionsPattern[ColorBarClass] ] :=
	Module[{attrs},
		o.type = "ColorBar";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* BoxAnnotationClass *)

BoxAnnotationClass = 
	NewClass[
		"Parents"	-> {AnnotationModelClass},
		"Fields"	-> {
			"left"				-> None						(* Either(Auto, NumberSpec(), default=None *)
			, "left_units"		-> "data"					(* Enum(SpatialUnits *)
			, "right"			-> None						(* Either(Auto, NumberSpec(), default=None *)
			, "right_units"		-> "data"					(* Enum(SpatialUnits, default='data' *)
			, "bottom"			-> None						(* Either(Auto, NumberSpec() *)
			, "bottom_units"	-> "data"					(* Enum(SpatialUnits, default='data' *)
			, "top"				-> None						(* Either(Auto, NumberSpec(), default=None *)
			, "top_units"		-> "data"					(* Enum(SpatialUnits, default='data' *)
			, "x_range_name"	-> "BString"["default"]
			, "y_range_name"	-> "BString"["default"]
			, IncludeProps[LineProps,
				"line_alpha"		-> "NumberSpec"[0.3]		(* override *)
				, "line_color"		-> "ColorSpec"["#cccccc"]	(* override *)
			]
			, IncludeProps[FillProps,
				"fill_alpha"		-> "NumberSpec"[0.4]		(* override *)
				, "fill_color"		-> "ColorSpec"["#fff9ba"]	(* override *)
			]
			, "render_mode"		-> "canvas"					(* Enum(RenderMode, default="canvas" *)
			}
	]
		
BoxAnnotationClass.init[opts : OptionsPattern[BoxAnnotationClass] ] :=
	Module[{attrs},
		o.type	= "BoxAnnotation";

		(* parse options *)

		(* attribute fields *)
		attrs = o.attributes;

		attrs."bottom_units"	= "screen";
		attrs."fill_color"		= <|"value" -> "lightgrey"|>;
		attrs."left_units"		= "screen";
		attrs."level"			= "overlay";
		attrs."line_color"		= <|"value" -> "black"|>;
		attrs."line_dash"		= {4, 4};
		attrs."line_width"		= <|"value" -> 2|>;
		attrs."plot"			= Null;
		attrs."render_mode"		= "css";
		attrs."right_units"		= "screen";
		attrs."top_units"		= "screen";
	]


(* ::Subsection:: *)
(* LabelModelClass *)

LabelModelClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
			"x"							-> Null		(* Float *)
(*			
    x = Float(help="""
    The x-coordinate in screen coordinates to locate the text anchors.

    Datetime values are also accepted, but note that they are immediately
    converted to milliseconds-since-epoch.
    """).accepts(Datetime, convert_datetime_type)
*)			, "x_units"					-> "data"
(*
    x_units = Enum(SpatialUnits, default='data', help="""
    The unit type for the x attribute. Interpreted as "data space" units
    by default.
    """)
*)
			, "y"						-> Null (* Float *)
			, "y_units"					-> "data"
			, "text"					-> "BString"[""]
			, "angle"					-> "Angle"[0]
			, "angle_units"				-> "rad"					(* Enum(AngleUnits, default='rad' *)
			, "x_offset"				-> "BFloat"[0]
			, "y_offset"				-> "BFloat"[0]
			, IncludeProps[TextProps]
			, IncludeProps[FillProps, "background",
				"background_fill_color"		-> "ColorSpec"[None] 	(* override *)
			]
			, IncludeProps[LineProps,
				"border_line_color"			-> "ColorSpec"[None]	(* override *)
			]
			, "x_range_name"			-> "BString"["default"]
			, "y_range_name"			-> "BString"["default"]
    		, "render_mode"				-> "canvas"					(* Enum(RenderMode, default="canvas" *)
		}
	]
	
LabelModelClass.init[opts : OptionsPattern[LabelModelClass] ] :=
	Module[{attrs},
		o.type = "Label";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* PolyAnnotationClass *)

PolyAnnotationClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
 			"xs"				-> {}
			, "xs_units"		-> "data"		(* Enum(SpatialUnits, default='data' *)
			, "ys"				-> {}
			, "ys_units"		-> "data"	
			, "x_range_name"	-> "BString"["default"]
			, "y_range_name"	-> "BString"["default"]
			, IncludeProps[LineProps,
				"line_alpha"		-> "NumberSpec"[0.3]		(* override *)
				, "line_color"		-> "ColorSpec"["#cccccc"]	(* override *)
			]
			, IncludeProps[FillProps,
				"fill_alpha"		-> "NumberSpec"[0.4] 		(* override *)
				, "fill_color"		-> "ColorSpec"["#fff9ba"]	(* override *)
			]
		}
	]
	
PolyAnnotationClass.init[opts : OptionsPattern[PolyAnnotationClass] ] :=
	Module[{attrs},
		o.type = "PolyAnnotation";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* TitleClass *)

TitleClass = 
	NewClass[
		"Parents"	-> {AnnotationModelClass},
		"Fields"	-> {
			"text" 						-> "BString"[""]
			, "align"					-> "left"					(* Enum(TextAlign, default='left' *)
			, "offset"					-> "BFloat"[0]
			, "text_font"				-> "BString"["helvetica"]
			, "text_font_size"			-> "FontSizeSpec"["10pt"]	(* FontSizeSpec(default=value("10pt"))*)
			, "text_font_style"			-> "bold"					(* Enum(FontStyle, default="bold" *)
			, "text_color"				-> "ColorSpec"["#444444"]	(* ColorSpec(default="#444444" *)
			, "text_alpha"				-> "NumberSpec"[1.0]		(* NumberSpec(default=1.0*)
			, IncludeProps[FillProps, "background",
				"background_fill_color"		-> "ColorSpec"[None]
			]
			, IncludeProps[LineProps, "border",
				"border_line_color"			-> "ColorSpec"[None]
			]
			, "render_mode"				-> "canvas"					(* Enum(RenderMode, default="canvas" *)
			}
	]
	
TitleClass.init[opts : OptionsPattern[TitleClass] ] :=
	Module[{attrs},
		o.type 	= "Title";

		If[o.plot === PlotModelClass, o.plot = Null ];

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* TooltipModelClass *)

TooltipModelClass =
	NewClass[
		"Parents"		-> {AnnotationModelClass},
		"Fields"		-> {
			"level"	-> "overlay" (* override *)
			, "attachment"	-> "right"
			, "inner_only"	-> True
			, "show_arrow"	-> True
		}
	]
	
TooltipModelClass.init[opts : OptionsPattern[TooltipModelClass] ] :=
	Module[{attrs},
		o.type = "Tooltip";

		(* attribute fields *)
		attrs = o.attributes;
	]


