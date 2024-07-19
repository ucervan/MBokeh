(* Wolfram Language package *)

(* ::Section:: *)
(* PlotModelClass *)
  
PlotModelClass = 
	NewClass[
		"Parents"	-> {LayoutDOMClass},
		"Fields"	-> {
				"subtype" 				-> None
				, "x_range"				-> "Instance"[RangeClass] 		(* RangeClass *)
				, "y_range"				-> "Instance"[RangeClass]		(* RangeClass *)
				, "x_scale"				-> "Instance"[ScaleClass]		(* Instance(Scale, default=lambda: LinearScale()) *)
				, "y_scale"				-> "Instance"[ScaleClass]		(* Instance(Scale, default=lambda: LinearScale() *)
				, "extra_x_ranges"		-> Null							(* Dict(String, Instance(Range) *)
				, "extra_y_ranges"		-> Null							(* Dict(String, Instance(Range) *)
				, "hidpi"				-> "BBool"[True]
				, "title"				-> "Instance"[TitleClass]		(* Instance(Title, default=lambda: Title(text="") ) *)
				, "title_location"		-> "above"						(* Enum(Location, default="above" *)
				, IncludeProps[LineProps, "outline",
					"outline_line_color"	-> "ColorSpec"["#e5e5e5"]	(* override *)
				]
				, "renderers"			-> List[BokehAttributeClass] 	(* BagClass *)
				, "toolbar"				-> "Instance"[ToolbarClass]		(* Instance(Toolbar *)
				, "toolbar_location"	-> "Enum"["right", EnumLocation](* Enum(Location, default="right" *)
				, "toolbar_sticky"		-> "BBool"[True]
				, "left"				-> List[BokehAttributeClass] 	(* BagClass *)
				, "right"				-> List[BokehAttributeClass]	(* BagClass *)
				, "above"				-> List[BokehAttributeClass]	(* BagClass *)
				, "below"				-> List[BokehAttributeClass] 	(* BagClass *)
				, "plot_height"			-> "BInt"[600]
				, "plot_width"			-> "BInt"[600]
				, IncludeProps[FillProps, "background",
					"background_fill_color"	-> "ColorSpec"["#ffffff"]
				]
				, IncludeProps[FillProps, "border",
					"border_fill_color"		-> "ColorSpec"["#ffffff"]					
				]
				, "min_border_top"		-> "BInt"[5]					(* review defaults *)
				, "min_border_bottom"	-> "BInt"[5]
				, "min_border_left"		-> "BInt"[5]
				, "min_border_right"	-> "BInt"[5]
				, "min_border"			-> "BInt"[5]
				, "h_symmetry"			-> "BBool"[True]
				, "v_symmetry"			-> "BBool"[True]
				, "lod_factor"			-> "BInt"[10]
				, "lod_threshold"		-> "BInt"[2000]
				, "lod_interval"		-> "BInt"[300]				
				, "lod_timeout"			-> "BInt"[500]
				, "output_backend"		-> "canvas"					(* Enum(OutputBackend, default="canvas" *)
				, "match_aspect"		-> "BBool"[False]
				, "aspect_scale"		-> "BFloat"[1]
			}
	]
		
PlotModelClass.init[opts : OptionsPattern[PlotModelClass] ] := 
	Module[{attrs, title},
		o.type = "Plot";

		(* Title *)
		title = o."title";
		If[title === TitleClass, title = ""];
		If[StringQ[title],
			o."title" = New[TitleClass]["text"->title]
			];
			
		(* Toolbar_location *)
		If[StringQ[o."toolbar_location"], 
			o."toolbar_location" = ToLowerCase[o."toolbar_location"]];
		o."toolbar_location" = 
			Switch[o."toolbar_location",
				(Left | "left")					, "left",
  				(Automatic | Right | "right")	, "right",
  				(Top | Above | "above")			, "above",
  				(Below | Bottom | "below")		, "below",
  				_, Null
  			];

		attrs = o.attributes;
	]
PlotModelClass.initInstance[opts : OptionsPattern[] ] :=
	Module[{tools, logo},
		(* Toolbar *)
		If[o."toolbar" === ToolbarClass,
			tools = "tools" /. {opts} /. {"tools"->{}};
			logo = "logo" /. {opts} /. {"logo"->"normal"};
			o."toolbar" = Toolbar["logo"->logo]
		];
	]
PlotModelClass.attributeID[] := <| "id"->o.id, "subtype"->o.subtype, "type"->o.type |>

PlotModelClass.$axis[sides__]:=
	Module[{bag, nsides},
		nsides = Intersection[{sides}, {"left","right","above","below"}];
		bag = Bag[];
		Scan[If[MatchQ[#, _BagClass], bag.stuff[#.all[]]]&, (o.#)& /@ nsides];
		ListAttrSplat[Select[bag.all[], IsInstanceQ[#, AxisModelClass] &]]
	]
PlotModelClass.xaxis[] := o.$axis["above", "below"]
PlotModelClass.yaxis[] := o.$axis["left", "right"]
PlotModelClass.axis[] := o.xaxis+o.yaxis
PlotModelClass.legend[] :=
	Module[{items},
		items = If[MatchQ[o.renderers, _BagClass], o.renderers.all[], {}];
		ListAttrSplat[Select[items, IsInstanceQ[#,LegendClass]&]]
	]
PlotModelClass.$grid[dim_Integer] :=
	Module[{items},
		items = If[IsInstanceQ[o.renderers, BagClass], o.renderers.all[], {}];
		ListAttrSplat[Select[items, (IsInstanceQ[#,GridModelClass] && #.dimension === dim)&]]
	]
PlotModelClass.xgrid[] := o.$grid[0]
PlotModelClass.ygrid[] := o.$grid[1]
PlotModelClass.grid[] := (o.xgrid[] + o.ygrid[])
PlotModelClass.tools[] := o."toolbar"."tools"
PlotModelClass.tools[tools_List] := o."toolbar"."tools" = Bag["content"->tools]
PlotModelClass.addlayout[obj_, place_:"center"] :=
	Module[{validplaces},
		validplaces = {"left", "right", "above", "below", "center"};
		If[!MemberQ[validplaces, place],
				ValueError["Invalid place \"`1`\" specified. Valid place values are: `2`", {place, validplaces}];
				Return[];
		];
		If[HasAttrQ[obj, "plot"],
			If[IsInstanceQ[obj.plot],
				ValueError["object to be added already has \"plot\" attribute set", {}];
				Return[]
			];
			obj.plot = o
		];
		o.renderers.stuff[obj];
		If[place =!="center",
			(o.place).stuff[obj]
		];
	]
PlotModelClass.addtools[tools_List] :=
	Module[{},
		Scan[(
			If[HasAttrQ[#, "overlay"], o.renderers.stuff[#."overlay"]];
			o."toolbar"."tools".stuff[#]
			)&, tools]
	]
PlotModelClass.addtools[tool_] := If[IsInstanceQ[tool, ToolClass], o.addtools[{tool}], $Failed]
PlotModelClass.addglyph[sourceorglyph_, oglyph_:None, opts___] :=
	Module[{source, glyph = oglyph, g},
		
		If[oglyph =!= None, (* then *)
			source = sourceorglyph, (* else *)
			{source, glyph}	= {ColumnDataSource[], sourceorglyph}
		];
		
		If[!IsInstanceQ[source, DataSourceClass], (* then *)
			ValueError["\"source\" argument to addglyph[] must be DataSourceClass subclass"];
			Return[]
		];

		If[!IsInstanceQ[glyph, GlyphClass], (* then *)
			ValueError["\"glyph\" argument to addglyph[] must be GlyphClass subclass"];
			Return[]
		];
		
		g = GlyphRenderer["data_source"->source, "glyph"-> glyph, opts];
		
		o.renderers.stuff[g]
	]
PlotModelClass.addtile[tilesource_, opts___] :=
	Module[{tilerenderer},
		tilerenderer = TileRenderer["tile_source"->tilesource, opts];
		o.renderers.stuff[tilerenderer];
		tilerenderer
	]
