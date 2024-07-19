(* Wolfram Language package *)

(*
			, LayoutDOMClass
				, SpacerBoxClass
				, WidgetClass
				, WidgetBoxClass
				, BoxModelClass
					, RowBoxModelClass
					, ColumnBoxClass
*)

(* ::Section:: *)
(* LayoutDOMClass *)

LayoutDOMClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
			"width"			-> "BInt"[Null]
			, "height"		-> "BInt"[Null]						(* Int(An optional height for the component (in pixels). *)
    		, "disabled"	-> "BBool"[False]
			, "sizing_mode"	-> "Enum"["fixed", EnumSizingMode]	(* Enum(SizingMode, default="fixed", *)
(*
    How the item being displayed should size itself. Possible values are
    ``"fixed"``, ``"scale_width"``, ``"scale_height"``, ``"scale_both"``, and
    ``"stretch_both"``.    
*)
			, "css_classes"	-> {}				(* List(String *)
		}
	]
	
LayoutDOMClass.init[opts : OptionsPattern[LayoutDOMClass] ] :=
	Module[{attrs},
		o.type = "LayoutDOM";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* SpacerBoxClass *)

(*
   A container for space used to fill an empty spot in a row or column.
*)
SpacerBoxClass =
	NewClass[
		"Parents"		-> {LayoutDOMClass},
		"Fields"		-> {
		}
	]
	
SpacerBoxClass.init[opts : OptionsPattern[SpacerBoxClass] ] :=
	Module[{attrs},
		o.type = "Spacer";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* WidgetClass *)

(*
   A container for space used to fill an empty spot in a row or column.
*)
WidgetClass =
	NewClass[
		"Parents"		-> {LayoutDOMClass},
		"Fields"		-> {
		}
	]
	
WidgetClass.init[opts : OptionsPattern[WidgetClass] ] :=
	Module[{attrs},
		o.type = "Widget";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* WidgetBoxClass *)

WidgetBoxClass =
	NewClass[
		"Parents"		-> {LayoutDOMClass},
		"Fields"		-> {
			"children"		-> List[BokehAttributeClass] (* List(Instance('bokeh.models.widgets.Widget'), *)
		}
	]
	
WidgetBoxClass.init[opts : OptionsPattern[WidgetBoxClass] ] :=
	Module[{attrs},
		o.type = "WidgetBox";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* BoxModelClass *)

BoxModelClass =
	NewClass[
		"Parents"		-> {LayoutDOMClass},
		"Fields"		-> {
			"children"		-> List[LayoutDOMClass] 	(* List(Instance(LayoutDOM), *)
		}
	]
	
BoxModelClass.init[opts : OptionsPattern[BoxModelClass] ] :=
	Module[{attrs},
		o.type = "Box";
		
		If[ListQ[o.children], o.children = o.wrapchildren[o.children]];

		(* attribute fields *)
		attrs = o.attributes;
	]
	
BoxModelClass.wrapchildren[children_List] :=
	Module[{wchildren = Bag[], child},
		Scan[
			(
			child = #;
			If[IsInstanceQ[#, WidgetClass],
				child = WidgetBox[ 
						"children"		-> {#},
						"sizing_mode"	-> (#."sizing_mode"),
						"width"			-> (#."width"),
						"height"		-> (#."height"),
						"disabled"		-> (#."disabled")
				];
				child."children".stuff[#]
			];
			wchildren.stuff[child])&, children];
		wchildren.all[]
	]


(* ::Subsection:: *)
(* RowBoxModelClass *)

RowBoxModelClass =
	NewClass[
		"Parents"		-> {BoxModelClass},
		"Fields"		-> {
		}
	]
	
RowBoxModelClass.init[opts : OptionsPattern[RowBoxModelClass] ] :=
	Module[{attrs},
		o.type = "Row";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ColumnBoxClass *)

ColumnBoxClass =
	NewClass[
		"Parents"		-> {BoxModelClass},
		"Fields"		-> {
		}
	]
	
ColumnBoxClass.init[opts : OptionsPattern[ColumnBoxClass] ] :=
	Module[{attrs},
		o.type = "Column";

		(* attribute fields *)
		attrs = o.attributes;
	]


verifysizingmode[v_] :=	If[!InEnumerationQ[v, EnumSizingMode],
			ValueError["Invalid value of sizing_mode: `1`", sizingmode];
			False,
			True
		]

(* ::Section:: *)
(* row *)

Options[row] = Join[{
	"sizing_mode"	-> "fixed"
}, Options[RowBoxModelClass]]
SetAttributes[row, HoldFirst]
row[children_List, opts : OptionsPattern[]] :=
	Module[{sizingmode, cbag=Bag[]},
		sizingmode = OptionValue["sizing_mode"];		
		If[!verifysizingmode[sizingmode], Return[$Failed] ];
		Scan[If[IsInstanceQ[#, LayoutDOMClass], (* then *)
			(#."sizing_mode") = sizingmode;
			cbag.stuff[#], (* else *)
			ValueError["Only LayoutDOM items can be inserted into a row.\nTried to insert: `1` of type `2`", {#, #.type[]}];
			Return[$Failed]
			]&, children];			
		New[RowBoxModelClass]["children"-> (cbag.all[]), opts ]
	]
row[children__, opts : OptionsPattern[] ] := row[{children}, opts]
row[opts___] := row[{}, opts]


(* ::Section:: *)
(* column *)

Options[column] = Join[{
	"sizing_mode"	-> "fixed"
}, Options[ColumnBoxClass]]
SetAttributes[column, HoldFirst]
column[children_List, opts : OptionsPattern[]] :=
	Module[{sizingmode, cbag = Bag[]},
		sizingmode = OptionValue["sizing_mode"];		
		If[!verifysizingmode[sizingmode], Return[$Failed] ];
		Scan[If[IsInstanceQ[#, LayoutDOMClass], (* then *)
			(#."sizing_mode") = sizingmode;
			cbag.stuff[#], (* else *)
			ValueError["Only LayoutDOM items can be inserted into a column.\nTried to insert: `1` of type `2`", {#, #.type[]}];
			Return[$Failed]
			]&, children];
		New[ColumnBoxClass]["children"-> (cbag.all[]), opts ]
	]
column[children__, opts : OptionsPattern[] ] := column[{children}, opts]
column[opts___] := column[{}, opts]


(* ::Section:: *)
(* widgetbox *)

(*
	Create a WidgetBox of Bokeh widgets. Forces all to
    have the same sizing_mode, which is required for complex layouts to work.

    Args:
        children (list of :class:`~bokeh.models.widgets.widget.Widget` ): A list
            of widgets for the WidgetBox.

        sizing_mode (``"fixed"``, ``"stretch_both"``, ``"scale_width"``, ``"scale_height"``, ``"scale_both"`` ): How
            will the items in the layout resize to fill the available space.
            Default is ``"fixed"``. For more information on the different
            modes see :attr:`~bokeh.models.layouts.LayoutDOM.sizing_mode`
            description on :class:`~bokeh.models.layouts.LayoutDOM`.

    Returns:
        WidgetBox: A WidgetBox of Widget instances all with the same sizing_mode.

    Examples:

        >>> widgetbox({button, select})
        >>> widgetbox(slider, sizing_mode->"scale_width")

*)

Options[widgetbox] = Join[{
	"sizing_mode"	-> "fixed"
}, Options[WidgetBoxClass]]

SetAttributes[widgetbox, HoldFirst]
widgetbox[children_List, opts : OptionsPattern[]] :=
	Module[{wbox, sizingmode},
		sizingmode = OptionValue["sizing_mode"];
		If[!verifysizingmode[sizingmode], Return[$Failed] ];

		wbox = WidgetBox[opts];
		Scan[If[IsInstanceQ[#, WidgetClass], 			
			wbox."children".stuff[#],
			(* else *)
			ValueError["Only Widgets can be inserted into a WidgetBox. \nTried to insert `1` of type `2`", {#,#.type[]}];
			Return[$Failed]
			]&, children];
		wbox
	]
widgetbox[children__, opts : OptionsPattern[] ] := widgetbox[{children}, opts]
widgetbox[opts___] 			:= widgetbox[{}, opts]


(* ::Section:: *)
(* layout *)

SetAttributes[creategrid, HoldFirst]
creategrid[iterable_List, sizingmode_, layer_:0] :=
	Module[{returnbag, children, rfunc},
		returnbag = Bag[];		
		Scan[(
			Which[
				ListQ[#],
					returnbag.stuff[ creategrid[#, sizingmode, layer+1], False ],
				IsInstanceQ[#, LayoutDOMClass],
					(#."sizing_mode" = sizingmode);
					returnbag.stuff[ # ],
				True,
					ValueError["Only LayoutDOM items can be inserted into a layout. \nTried to insert:`1` of type `2`", 
							{#. (#.type[])}];
					Return[$Failed]
			]
		)&, iterable];
		children = returnbag.all[];
		rfunc = If[Mod[layer,2] === 0, column, row ];
		rfunc @@ {children, "sizing_mode"->sizingmode}
	]

Options[layout] = Join[{
	"sizing_mode"	-> "fixed"
}, {}]

SetAttributes[layout, HoldFirst]
layout[children_List, opts : OptionsPattern[]] :=
	Module[{sizingmode},
		sizingmode = OptionValue["sizing_mode"];
		If[!verifysizingmode[sizingmode], Return[$Failed] ];
		creategrid[children, sizingmode]
	]

(* ::Section:: *)
(* gridplot *)

(*
	Create a grid of plots rendered on separate canvases. gridplot[] builds a single toolbar
    for all the plots in the grid. gridplot[] is designed to layout a set of plots. For general
    grid layout, use the :func:`~bokeh.layouts.layout` function.
*)
Options[gridplot] = {
	"children"				-> None
	, "sizing_mode"			-> "fixed"
	, "toolbar_location"	-> "above"
	, "toolbar_options"		-> <||>
	, "ncols"				-> None
	, "plot_width"			-> None
	, "plot_height"			-> None
	, "merge_tools"			-> True
}

SetAttributes[gridplot, HoldFirst]
gridplot[children_List, opts : OptionsPattern[] ] := gridplot@@{Sequence@@(Prepend[{opts}, "children"->children])}
gridplot[opts : OptionsPattern[] ] :=
	Module[{toolbarlocation, sizingmode, children, 
		toolbaroptions, plotwidth, plotheight, ncols, mergetools, tmp,
		tools, rows, grid, proxy, toolbar, rowtools, rowchildren, width, height, 
		lrow, item, cont, failed },
		{toolbarlocation, sizingmode, children, toolbaroptions, plotwidth, plotheight, ncols, mergetools} = 
			OptionValue[{"toolbar_location", "sizing_mode", "children", 
				"toolbar_options", "plot_width", "plot_height", "ncols", "merge_tools"}];
	
		(* check valid sizingmode *)
		If[!verifysizingmode[sizingmode], Return[$Failed] ];
				
		If[toolbarlocation=!=None,
			If[!InEnumerationQ[toolbarlocation, "Location"],
				ValueError["Invalid value of toolbar_location: `1`", toolbarlocation];
				Return[$Failed]
			]		
		];
		
		If[IntegerQ[ncols],
			failed = False;
			Scan[If[ListQ[#],
					ValueError["Cannot provide a nested list when using ncols"];
					failed = True;
					Return[$Failed];
				]& , children];
			If[failed, Return[$Failed]];
			tmp = Partition[children, ncols];
			children = If[Mod[Length[children], ncols]===0, tmp, Append[tmp, Take[children, -(Length[children] - ncols * Length[tmp])]]]			
		];
		
		tools = Bag[]; rows = Bag[];
		Scan[(
			lrow = #; cont = True;
			If[!ListQ[lrow],
				ValueError["`1` is not a list.", lrow];
				cont = False;
				Return[];
			];
			rowtools = Bag[];
			rowchildren= Bag[];
			Scan[(
				item = #;
				If[mergetools,
					If[!MatchQ[item, (None | Null)],
						Scan[(
							tmp =item."toolbar"."tools";
							rowtools.stuff[tmp.all[]];
							#."toolbar_location" = Null
						)&, (#.select[PlotModelClass,None])."data"]
					]
				];
				If[MatchQ[item, (None | Null)],
					{width, height} = {0,0};
					cont = True;
					Scan[(
						If[cont && IsInstanceQ[#, PlotModelClass],
							width = #."plot_width";
							height = #."plot_height";							
							cont = False							
						]
					)&, lrow];
					item = SpacerBox["width"->width, "height"->height]
				];	
				If[IsInstanceQ[item, LayoutDOMClass], (* then *)
					item."sizing_mode" = sizingmode;
					If[IsInstanceQ[item, PlotModelClass],
						If[plotwidth =!= None, item."plot_width" = plotwidth];
						If[plotheight =!= None, item."plot_height" = plotheight]						
					];
					rowchildren.stuff[item, False], (* else *)
					ValueError["Only LayoutDOM items can be inserted into Grid"];
					Return[$Failed]
				]
			)&, #];
			tools.stuff[rowtools.all[]];
			rows.stuff[New[RowBoxModelClass]["children"->(rowchildren.all[]), "sizing_mode"->sizingmode], False]
			)&, children
		];
		If[!cont, Return[$Failed]];

		grid = New[ColumnBoxClass]["children"->rows.all[], "sizing_mode"->sizingmode];
		If[!mergetools, Return[grid]];

		If[toolbarlocation =!= None,
			proxy = ProxyToolbar["tools"-> tools.all[] (*, Sequence@@Normal[toolbaroptions] *)];
			toolbar = ToolbarBox["toolbar"->proxy, "toolbar_location"->toolbarlocation]
		];
		Switch[toolbarlocation,
			"above", New[ColumnBoxClass]["children"->{toolbar, grid}, "sizing_mode"-> sizingmode],
			"below", New[ColumnBoxClass]["children"->{grid, toolbar}, "sizing_mode"-> sizingmode],
			"left" , New[RowBoxModelClass]["children"->{toolbar, grid}, "sizing_mode"-> sizingmode],
			"right", New[RowBoxModelClass]["children"->{grid, toolbar}, "sizing_mode"-> sizingmode],
			_, grid			
		]
	]


