(* Wolfram Language package *)

(*
	Model
		, RendererClass
			, DataRendererClass
				, TileRendererClass
				, DynamicImageRendererClass
				, GlyphRendererClass
				, GraphRendererClass
			, GuideRendererClass
				, GridModelClass
*)


(* ::Section:: *)
(* RendererClass *)

RendererClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
			"level"		-> "Enum"[Null, EnumRenderLevel]
			, "visible"	-> "BBool"[True]
		}
	]
	
RendererClass.init[opts : OptionsPattern[RendererClass] ] :=
	Module[{attrs},
		o.type = "Renderer";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DataRendererClass *)

(* abstract *)
DataRendererClass =
	NewClass[
		"Parents"		-> {RendererClass},
		"Fields"		-> {
		}
	]
	
DataRendererClass.init[opts : OptionsPattern[DataRendererClass] ] :=
	Module[{attrs},
		o.type = "DataRenderer";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* TileRendererClass *)

TileRendererClass =
	NewClass[
		"Parents"		-> {DataRendererClass},
		"Fields"		-> {
			"tile_source"	-> "Instance"[TileSourceClass] (*, default=lambda: WMTSTileSource(), help="""
    Local data source to use when rendering glyphs on the plot.
    """)
*)
			, "alpha"		-> "BFloat"[1.0]
			, "x_range_name"	-> "BString"["default"]
			, "y_range_name"	-> "BString"["default"]
			, "level"			-> "Enum"["underlay", EnumRenderLevel]
			, "render_parents"	-> "BBool"[True]
		}
	]
	
TileRendererClass.init[opts : OptionsPattern[TileRendererClass] ] :=
	Module[{attrs},
		o.type = "TileRenderer";
		
		If[o."tile_source" === TileSourceClass,
			o."tile_source" = WMTSTileSource[]
		];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* DynamicImageRendererClass *)

DynamicImageRendererClass =
	NewClass[
		"Parents"		-> {DataRendererClass},
		"Fields"		-> {
		}
	]
	
DynamicImageRendererClass.init[opts : OptionsPattern[DynamicImageRendererClass] ] :=
	Module[{attrs},
		o.type = "DynamicImageRenderer";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphRendererClass *)

GlyphRendererClass =
	NewClass[
		"Parents"		-> {DataRendererClass},
		"Fields"		-> {
			"data_source"			-> "Instance"[DataSourceClass]
			, "view"				-> "Instance"[CDSViewClass]
			, "x_range_name"		-> "BString"["default"]
			, "y_range_name"		-> "BString"["default"]
			, "glyph"				-> "Instance"[GlyphClass]
			, "selection_glyph"		-> "Instance"[GlyphClass]	(* Either(Auto, Instance(Glyph), default="auto",  *)
			, "nonselection_glyph" 	-> "Instance"[GlyphClass]	(* Either(Auto, Instance(Glyph), default="auto", *)
			, "hover_glyph"			-> "Instance"[GlyphClass]
			, "muted_glyph"			-> "Instance"[GlyphClass]
			, "muted"				-> "BBool"[False]
			, "level"				-> "glyph"
		}
	]
	
GlyphRendererClass.init[opts : OptionsPattern[GlyphRendererClass] ] :=
	Module[{attrs},
		o.type = "GlyphRenderer";
		
		If[o."view" === CDSViewClass,
			o.view = CDSView["source"->o."data_source"]
		];
		
		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsubsection:: *)
(* GraphRendererClass *)

GraphRendererClass =
	NewClass[
		"Parents"		-> {DataRendererClass},
		"Fields"		-> {
		}
	]
	
GraphRendererClass.init[opts : OptionsPattern[GraphRendererClass] ] :=
	Module[{attrs},
		o.type = "GraphRenderer";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* GuideRendererClass *)

GuideRendererClass =
	NewClass[
		"Parents"		-> {RendererClass},
		"Fields"		-> {
			"plot"		-> "Instance"[PlotModelClass]
			, "level"	-> "overlay" (* override *)
		}
	]
	
GuideRendererClass.init[opts : OptionsPattern[GuideRendererClass] ] :=
	Module[{attrs},
		o.type = "GuideRenderer";

		Switch[o."plot",
			PlotModelClass, 
				o."plot" = None,
			MBokehClassPatterns,
				If[MatchQ[o."plot".renderers, _BagClass], (* then *)
					o."plot".renderers.stuff[o]
				],
			_, 
				o."plot"=None
		];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* GridModelClass *)

GridModelClass =
	NewClass[
		"Parents"		-> {GuideRendererClass},
		"Fields"		-> {
			"dimension"					-> "BInt"[0]
			, "bounds"					-> "auto"
			, "x_range_name"			-> "BString"["default"]
			, "y_range_name"			-> "BString"["default"]
			, "ticker"					-> "Instance"[TickerClass] 		(* Instance(Ticker) *) 
			, IncludeProps[LineProps, "grid",
				"grid_line_color"			-> "ColorSpec"["#e5e5e5"] 	(* override *)
			]
			, IncludeProps[LineProps, "minor_grid",
				"minor_grid_line_color"		-> "ColroSpec"[None]		(* override *)
			]
			, IncludeProps[FillProps, "band",
				"band_fill_alpha"			-> "NumberSpec"[0]			(* override *)
				, "band_fill_color"			-> "ColorSpec"[None] 		(* override *)
			]
			, "level"					-> "underlay"					(* override *)
		}
	]
	
GridModelClass.init[opts : OptionsPattern[GridModelClass] ] :=
	Module[{attrs},
		o.type = "Grid";

		(* attribute fields *)
		attrs = o.attributes;
	]

