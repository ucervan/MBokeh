(* Wolfram Language package *)

(*
	LayoutDOMClass
		, ToolbarBoxClass

	Model
		, ToolbarBaseClass
			, ToolbarClass
			, ProxyToolbarClass
		, ToolClass
			, ActionClass
			, DragClass
			, ScrollClass
			, TapClass
			, InspectionClass
		, PanToolClass
		, WheelPanToolClass
		, WheelZoomToolClass
		, SaveToolClass
		, ResetToolClass
		, TapToolClass
		, CrosshairToolClass
		, BoxZoomToolClass
		, ZoomInToolClass
		, ZoomOutToolClass
		, BoxSelectToolClass
		, LassoSelectToolClass
		, PolySelectToolClass
		, CustomJSHoverClass
		, HoverToolClass
		, HelpToolClass
		, UndoToolClass
		, RedoToolClass
		, EditToolClass
			, BoxEditToolClass
			, PointDrawToolClass
			, PolyDrawToolClass
			, PolyEditToolClass
*)


(* ::Section:: *)
(* Tools *)

(* ::Subsection:: *)
(* ToolbarBaseClass *)

ToolbarBaseClass = 
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			"logo"					-> "normal"			(* "normal" | "grey" | None *)
			, "tools"				-> List[ToolClass]	(* List(Instance(Tool) *)
			}
	]

ToolbarBaseClass.init[opts : OptionsPattern[ToolbarBaseClass] ] :=
	Module[{attrs},
		o.type	= "ToolbarBase";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ProxyToolbarClass *)
(*
	A toolbar that allow to merge and proxy tools of toolbars in multiple plots. '''
*)
ProxyToolbarClass = 
	NewClass[
		"Parents"	-> {ToolbarBaseClass},
		"Fields"	-> {
			}
	]

ProxyToolbarClass.init[opts : OptionsPattern[ProxyToolbarClass] ] :=
	Module[{attrs},
		o.type	= "ProxyToolbar";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* ToolbarBoxClass *)
(*
	A layoutable toolbar that can accept the tools of multiple plots, and
    can merge the tools into a single button for convenience.
*)
ToolbarBoxClass = 
	NewClass[
		"Parents"	-> {LayoutDOMClass},
		"Fields"	-> {
			"toolbar"			-> "Instance"[ToolbarBaseClass]	(* Instance(ToolbarBase, *)
			, "toolbar_location"-> "Enum"["right", EnumLocation]
			}
	]

ToolbarBoxClass.init[opts : OptionsPattern[ToolbarBoxClass] ] :=
	Module[{attrs},
		o.type	= "ToolbarBox";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ToolClass *)

ToolClass = 
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			}
	]

ToolClass.init[opts : OptionsPattern[ToolClass] ] :=
	Module[{attrs},
		o.type	= "Tool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ActionClass *)

ActionClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
			}
	]

ActionClass.init[opts : OptionsPattern[ActionClass] ] :=
	Module[{attrs},
		o.type	= "Action";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DragClass *)

DragClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
			}
	]

DragClass.init[opts : OptionsPattern[DragClass] ] :=
	Module[{attrs},
		o.type	= "Drag";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ScrollClass *)

ScrollClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
			}
	]

ScrollClass.init[opts : OptionsPattern[ScrollClass] ] :=
	Module[{attrs},
		o.type	= "ScrollClass";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* TapClass *)

TapClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
			}
	]

TapClass.init[opts : OptionsPattern[TapClass] ] :=
	Module[{attrs},
		o.type	= "Tap";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* InspectionClass *)

InspectionClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
			"toggleable"	-> "BBool"[True] 	(* Bool(True, *)
(*
    Whether an on/off toggle button should appear in the toolbar for this
    inpection tool. If ``False``, the viewers of a plot will not be able to
    toggle the inspector on or off using the toolbar.
*)			}
	]

InspectionClass.init[opts : OptionsPattern[InspectionClass] ] :=
	Module[{attrs},
		o.type	= "Inspection";

		If[o.callback === CallbackClass, o.callback = Null];

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* PanToolClass *)

PanToolClass = 
	NewClass[
		"Parents"	-> {DragClass},
		"Fields"	-> {
				"dimensions"	-> "Enum"["both", EnumDimensions]
			}
	]

PanToolClass.init[opts : OptionsPattern[PanToolClass] ] :=
	Module[{attrs},
		o.type	= "PanTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* WheelPanToolClass *)

WheelPanToolClass = 
	NewClass[
		"Parents"	-> {ScrollClass},
		"Fields"	-> {
				"dimension"	-> "Enum"["width", EnumDimension]
			}
	]

WheelPanToolClass.init[opts : OptionsPattern[WheelPanToolClass] ] :=
	Module[{attrs},
		o.type	= "WheelPanTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* WheelZoomToolClass *)

WheelZoomToolClass = 
	NewClass[
		"Parents"	-> {ScrollClass},
		"Fields"	-> {
				"dimensions"		-> "Enum"["both", EnumDimensions]
				, "maintain_focus"	-> "BBool"[True] (* Bool(default=True, *)
(*
    Whether or not zooming tool maintains its focus position. Setting it
    to False results in a more "gliding" behavior, allowing one to
    zoom out more smoothly, at the cost of losing the focus position.
*)
				,"zoom_on_axis"		-> "BBool"[True] 	(* Bool(default=True, *)
(*
    Whether scrolling on an axis (outside the central plot area) should
    zoom that dimension.
*)
				, "speed"			-> "BFloat"[0.002]	(* Float(default=1/600, *)
(*
    Speed at which the wheel zooms. Default is 1/600. Optimal range is between
    0.001 and 0.09. High values will be clipped. Speed may very between browsers.
*)			}
	]

WheelZoomToolClass.init[opts : OptionsPattern[WheelZoomToolClass] ] :=
	Module[{attrs},
		o.type	= "WheelZoomTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* SaveToolClass *)

SaveToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			}
	]

SaveToolClass.init[opts : OptionsPattern[SaveToolClass] ] :=
	Module[{attrs},
		o.type = "SaveTool";
		
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* ResetToolClass *)

ResetToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			"reset_size"	-> True
			}
	]

ResetToolClass.init[opts : OptionsPattern[ResetToolClass] ] :=
	Module[{attrs},
		o.type	= "ResetTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* TapToolClass *)

TapToolClass = 
	NewClass[
		"Parents"	-> {TapClass},
		"Fields"	-> {
			"names"			-> {}
			, "renderers" 	-> List[RendererClass]
			, "behavior"	-> "Enum"["select", {"select", "inspect"}]
			, "callback"	-> "Instance"[CallbackClass]	(*	Instance(Callback) *)
			}
	]

TapToolClass.init[opts : OptionsPattern[TapToolClass] ] :=
	Module[{attrs},
		o.type	= "TapTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* CrosshairToolClass *)

CrosshairToolClass = 
	NewClass[
		"Parents"	-> {InspectionClass},
		"Fields"	-> {
			"dimensions"	-> "Enum"["both", EnumDimensions]
			, "line_color"	-> "BColor"["black"] (* Acceptable values are:
    - any of the 147 named `CSS colors`_, e.g ``'green'``, ``'indigo'``
    - an RGB(A) hex value, e.g., ``'#FF0000'``, ``'#44444444'``
    - a 3-tuple of integers (r,g,b) between 0 and 255
    - a 4-tuple of (r,g,b,a) where r,g,b are integers between 0..255 and a is between 0..1
    .. _CSS colors: http://www.w3schools.com/cssref/css_colornames.asp
    *)
    		, "line_width"	-> "BFloat"[1]
			, "line_alpha"	-> "BFloat"[1.0]
			}
	]

CrosshairToolClass.init[opts : OptionsPattern[CrosshairToolClass] ] :=
	Module[{attrs},
		o.type	= "CrosshairTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* BoxZoomToolClass *)

$DEFAULTBOXOVERLAYOPTS = {
    "level"			-> "overlay",
    "render_mode"	-> "css",
    "top_units"		-> "screen",
    "left_units"	-> "screen",
    "bottom_units"	-> "screen",
    "right_units"	-> "screen",
    "fill_color"	-> "lightgray",
    "fill_alpha"	-> 0.5,
    "line_color"	-> "black",
    "line_alpha"	-> 1.0,
    "line_width"	-> 2,
    "line_dash"		-> {4, 4}
}

BoxZoomToolClass = 
	NewClass[
		"Parents"	-> {DragClass},
		"Fields"	-> {
			"dimensions"		-> "Enum"["both", EnumDimensions]
			, "overlay"			-> "Instance"[BoxAnnotationClass] (* Instance(BoxAnnotation, default=DEFAULT_BOX_OVERLAY *)
			, "match_aspect"	-> "BBool"[False]
			, "origin"			-> "Enum"["corner", {"corner", "center"}]
			}
	]

BoxZoomToolClass.init[opts : OptionsPattern[BoxZoomToolClass] ] :=
	Module[{attrs},
		o.type	= "BoxZoomTool";

		If[o.overlay === BoxAnnotationClass,
			o.overlay = BoxAnnotation @@ $DEFAULTBOXOVERLAYOPTS
		];

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* ZoomInToolClass *)

ZoomInToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			"dimensions"	-> "Enum"["both", EnumDimensions]
			, "factor"		-> "BPercent"[0.1]
			}
	]

ZoomInToolClass.init[opts : OptionsPattern[ZoomInToolClass] ] :=
	Module[{attrs},
		o.type	= "ZoomInTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* ZoomOutToolClass *)

ZoomOutToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			"dimensions"	-> "Enum"["both", EnumDimensions]
			, "factor"		-> "BPercent"[0.1]
			}
	]

ZoomOutToolClass.init[opts : OptionsPattern[ZoomOutToolClass] ] :=
	Module[{attrs},
		o.type	= "ZoomOutTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* BoxSelectToolClass *)

BoxSelectToolClass = 
	NewClass[
		"Parents"	-> {DragClass},
		"Fields"	-> {
			"names"						-> {}
			, "renderers" 				-> List[RendererClass]
			, "select_every_mousemove"	-> "BBool"[False]
			, "dimensions"				-> "Enum"["both", EnumDimensions]
			, "callback"				-> "Instance"[CallbackClass] (*Instance(Callback) *)
			, "overlay"					-> "Instance"[BoxAnnotationClass]
			}
	]

BoxSelectToolClass.init[opts : OptionsPattern[BoxSelectToolClass] ] :=
	Module[{attrs},
		o.type	= "BoxSelectTool";

		If[o."callback" === CallbackClass, o."callback"=Null];
		If[o.overlay === BoxAnnotationClass,
			o.overlay = BoxAnnotation @@ $DEFAULTBOXOVERLAYOPTS
		];

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* LassoSelectToolClass *)

$DEFAULTPOLYOVERLAYOPTS = {
    "level"			-> "overlay",
    "xs_units"		-> "screen",
    "ys_units"		-> "screen",
    "fill_color"	-> "lightgray",
    "fill_alpha"	-> 0.5,
    "line_color"	-> "black",
    "line_alpha"	-> 1.0,
    "line_width"	-> 2,
    "line_dash"		-> {4, 4}
}

LassoSelectToolClass = 
	NewClass[
		"Parents"	-> {DragClass},
		"Fields"	-> {
			"names"						-> {}
			, "renderers" 				-> List[RendererClass]
			, "select_every_mousemove"	-> "BBool"[True]
			, "dimensions"				-> "Enum"["both", EnumDimensions]
			, "callback"				-> "Instance"[CallbackClass] (*Instance(Callback) *)
			, "overlay"					-> "Instance"[PolyAnnotationClass]
			}
	]

LassoSelectToolClass.init[opts : OptionsPattern[LassoSelectToolClass] ] :=
	Module[{attrs},
		o.type	= "LassoSelectTool";
	
		If[o."callback" === CallbackClass, o."callback"=Null];
		If[o.overlay === PolyAnnotationClass,
			o.overlay = PolyAnnotation @@ $DEFAULTPOLYOVERLAYOPTS
		];

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* PolySelectToolClass *)

PolySelectToolClass = 
	NewClass[
		"Parents"	-> {TapClass},
		"Fields"	-> {
			"names"						-> {}
			, "renderers" 				-> List[RendererClass]
			, "callback"				-> "Instance"[CallbackClass] (*Instance(Callback) *)
			, "overlay"					-> "Instance"[PolyAnnotationClass]
			}
	]

PolySelectToolClass.init[opts : OptionsPattern[PolySelectToolClass] ] :=
	Module[{attrs},
		o.type	= "PolySelectTool";

		If[o."callback" === CallbackClass, o."callback"=Null];
		If[o.overlay === PolyAnnotationClass,
			o.overlay = PolyAnnotation @@ $DEFAULTPOLYOVERLAYOPTS
		];

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CustomJSHoverClass *)

(*
	Define a custom formatter to apply to a hover tool field.

    Example:

        As an example, the following code adds a custom formatter to format
        WebMercator northing coordinates (in meters) as a latitude:

        .. code-block:: python

            lat_custom = CustomJSHover(code="""
                var projections = require("core/util/projections");
                var x = special_vars.x
                var y = special_vars.y
                var coords = projections.wgs84_mercator.inverse([x, y])
                return "" + coords[1]
            """)

            p.add_tools(HoverTool(
                tooltips=[( 'lat','@y{custom}' )],
                formatter=dict(y=lat_custom)
            ))


*)
CustomJSHoverClass = 
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			"args"	->	"Dict"[<||>] (* Dict(String, Instance(Model), *)
(*
    A mapping of names to Bokeh plot objects. These objects are made
    available to the callback code snippet as the values of named
    parameters to the callback.
*)
			, "code"	-> "" 		(*String(default="", *)
(*
    A snippet of JavaScript code to transform a single value. The variable
    ``value`` will contain the untransformed value and can be expected to be
    present in the function namespace at render time. Additionally, the
    variable ``special_vars`` will be available, and will provide a dict
    with the following contents:

    * ``x`` data-space x-coordinate of the mouse
    * ``y`` data-space y-coordinate of the mouse
    * ``sx`` screen-space x-coordinate of the mouse
    * ``sy`` screen-space y-coordinate of the mouse
    * ``data_x`` data-space x-coordinate of the hovered glyph
    * ``data_y`` data-space y-coordinate of the hovered glyph
    * ``indices`` column indices of all currently hovered glyphs

    If the hover is over a "multi" glyph such as ``Patches`` or ``MultiLine``
    then a ``segment_index`` key will also be present.

    Finally, the value of the format passed in the tooltip specification is
    available as the ``format`` variable.

    The snippet will be made into the body of a function and therefore requires
    a return statement.

    Example:

        .. code-block:: javascript

            code = '''
            return value + " total"
            '''
*)			}
	]

CustomJSHoverClass.init[opts : OptionsPattern[CustomJSHoverClass] ] :=
	Module[{attrs},
		o.type	= "CustomJSHover";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* HoverToolClass *)

(*

    The hover tool is a passive inspector tool. It is generally on at
    all times, but can be configured in the inspector's menu associated
    with the *toolbar icon* shown above.

    By default, the hover tool displays informational tooltips whenever
    the cursor is directly over a glyph. The data to show comes from the
    glyph's data source, and what is to be displayed is configurable with
    the ``tooltips`` attribute that maps display names to columns in the
    data source, or to special known variables.

    Here is an example of how to configure and use the hover tool::

        # Add tooltip (name, field) pairs to the tool. See below for a
        # description of possible field values.
        hover.tooltips = [
            ("index", "$index"),
            ("(x,y)", "($x, $y)"),
            ("radius", "@radius"),
            ("fill color", "$color[hex, swatch]:fill_color"),
            ("foo", "@foo"),
            ("bar", "@bar"),
            ("baz", "@baz{safe}"),
            ("total", "@total{$0,0.00}"
        ]

    You can also supply a ``Callback`` to the HoverTool, to build custom
    interactions on hover. In this case you may want to turn the tooltips
    off by setting ``tooltips=None``.

*)

HoverToolClass = 
	NewClass[
		"Parents"	-> {InspectionClass},
		"Fields"	-> {
			"names"						-> {}
			, "renderers" 				-> List[RendererClass]
			, "callback"				-> "Instance"[CallbackClass] 	(*Instance(Callback) *)
			, "tooltips"				-> {
											{"index"		, "$index" },
											{"data (x, y)"	, "($x, $y)" },
											{"screen (x, y)", "($sx, $sy)"}
											} (* Either(String, List(Tuple(String, String)),*)
(*											
    The (name, field) pairs describing what the hover tool should
    display when there is a hit.

    Field names starting with "@" are interpreted as columns on the
    data source. For instance, "@temp" would look up values to display
    from the "temp" column of the data source.

    Field names starting with "$" are special, known fields:

    :$index: index of selected point in the data source
    :$x: x-coordinate under the cursor in data space
    :$y: y-coordinate under the cursor in data space
    :$sx: x-coordinate under the cursor in screen (canvas) space
    :$sy: y-coordinate under the cursor in screen (canvas) space
    :$color: color data from data source, with the syntax:
        ``$color[options]:field_name``. The available options
        are: 'hex' (to display the color as a hex value), and
        'swatch' to also display a small color swatch.

    Field names that begin with ``@`` are associated with columns in a
    ``ColumnDataSource``. For instance the field name ``"@price"`` will
    display values from the ``"price"`` column whenever a hover is triggered.
    If the hover is for the 17th glyph, then the hover tooltip will
    correspondingly display the 17th price value.

    Note that if a column name contains spaces, the it must be supplied by
    surrounding it in curly braces, e.g. ``@{adjusted close}`` will display
    values from a column named ``"adjusted close"``.

    By default, values for fields (e.g. ``@foo``) are displayed in a basic
    numeric format. However it is possible to control the formatting of values
    more precisely. Fields can be modified by appending a format specified to
    the end in curly braces. Some examples are below.

    .. code-block:: python

        "@foo{0,0.000}"    # formats 10000.1234 as: 10,000.123

        "@foo{(.00)}"      # formats -10000.1234 as: (10000.123)

        "@foo{($ 0.00 a)}" # formats 1230974 as: $ 1.23 m

    Specifying a format ``{safe}`` after a field name will override automatic
    escaping of the tooltip data source. Any HTML tags in the data tags will
    be rendered as HTML in the resulting HoverTool output. See
    :ref:`custom_hover_tooltip` for a more detailed example.

    ``None`` is also a valid value for tooltips. This turns off the
    rendering of tooltips. This is mostly useful when supplying other
    actions on hover via the callback property.

    .. note::
        The tooltips attribute can also be configured with a mapping type,
        e.g. ``dict`` or ``OrderedDict``. However, if a ``dict`` is used,
        the visual presentation order is unspecified.

    """).accepts(Dict(String, String), lambda d: list(d.items()))

*)
			, "formatters"				-> <||> (* Dict(String, Either(Enum(TooltipFieldFormatter), Instance(CustomJSHover)), default=lambda: dict() *)
(*
    Specify the formatting scheme for data source columns, e.g.

    .. code-block:: python

        tool.formatters = dict(date="datetime")

    will cause format specifications for the "date" column to be interpreted
    according to the "datetime" formatting scheme. The following schemed are
    available:

    :``"numeral"``:
        Provides a wide variety of formats for numbers, currency, bytes, times,
        and percentages. The full set of formats can be found in the
        |NumeralTickFormatter| reference documentation.

    :``"datetime"``:
        Provides formats for date and time values. The full set of formats is
        listed in the |DatetimeTickFormatter| reference documentation.

    :``"printf"``:
        Provides formats similar to C-style "printf" type specifiers. See the
        |PrintfTickFormatter| reference documentation for complete details.

    If no formatter is specified for a column name, the default ``"numeral"``
    formatter is assumed.

    .. |NumeralTickFormatter| replace:: :class:`~bokeh.models.formatters.NumeralTickFormatter`
    .. |DatetimeTickFormatter| replace:: :class:`~bokeh.models.formatters.DatetimeTickFormatter`
    .. |PrintfTickFormatter| replace:: :class:`~bokeh.models.formatters.PrintfTickFormatter`

*)		
			, "mode"					-> "Enum"["mouse"		, {"mouse", "hline", "vline"}]
			, "point_policy"			-> "Enum"["snap_to_data", {"snap_to_data", "follow_mouse", "none"}]
			, "line_policy"				-> "Enum"["nearest"		, {"prev", "next", "nearest", "interp", "none"}]
			, "anchor"					-> "Enum"["center"		, EnumAnchor]
			, "attachment"				-> "Enum"["horizontal"	, EnumTooltipAttachment]
			, "show_arrow"				-> "BBool"[True]
			}
	]

HoverToolClass.init[opts : OptionsPattern[HoverToolClass] ] :=
	Module[{attrs, bag},
		o.type	= "HoverTool";

		If[o."callback" === CallbackClass, o."callback"=Null];
		If[!IsInstanceQ[o."renderers", BagClass], 
			bag = Bag[];
			bag.stuff[o."renderers"];
			o."renderers" = bag
		];
		
		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* HelpToolClass *)

$DEFAULTHELPTIP = "Click the question mark to learn more about Bokeh plot tools."
$DEFAULTHELPURL = "https://bokeh.pydata.org/en/latest/docs/user_guide/tools.html#built-in-tools"

HelpToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			"help_tooltip"	-> $DEFAULTHELPTIP
			, "redirect"	-> $DEFAULTHELPURL
			}
	]

HelpToolClass.init[opts : OptionsPattern[HelpToolClass] ] :=
	Module[{attrs},
		o.type	= "HelpTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* UndoToolClass *)

UndoToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			}
	]

UndoToolClass.init[opts : OptionsPattern[UndoToolClass] ] :=
	Module[{attrs},
		o.type	= "UndoTool";

		(* parse options *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* RedoToolClass *)

RedoToolClass = 
	NewClass[
		"Parents"	-> {ActionClass},
		"Fields"	-> {
			}
	]

RedoToolClass.init[opts : OptionsPattern[RedoToolClass] ] :=
	Module[{attrs},
		o.type	= "RedoTool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* EditToolClass *)

EditToolClass = 
	NewClass[
		"Parents"	-> {ToolClass},
		"Fields"	-> {
				"empty_value"	-> Null (* Either(Bool, Int, Float, Date, Datetime, Color, *)
				, "renderers"	-> List[RendererClass]				
			}
	]

EditToolClass.init[opts : OptionsPattern[EditToolClass] ] :=
	Module[{attrs},
		o.type	= "EditTool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* BoxEditToolClass *)

BoxEditToolClass = 
	NewClass[
		"Parents"	-> {EditToolClass, DragClass, TapClass},
		"Fields"	-> {
				"dimensions"	-> "Enum"["both", EnumDimensions]
			}
	]

BoxEditToolClass.init[opts : OptionsPattern[BoxEditToolClass] ] :=
	Module[{attrs},
		o.type	= "BoxEditToolClass";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* PointDrawToolClass *)

PointDrawToolClass = 
	NewClass[
		"Parents"	-> {EditToolClass, DragClass, TapClass},
		"Fields"	-> {
				"add"		-> "BBool"[True]
				, "drag"	-> "BBool"[True]
			}
	]

PointDrawToolClass.init[opts : OptionsPattern[PointDrawToolClass] ] :=
	Module[{attrs},
		o.type	= "PointDrawTool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* PolyDrawToolClass *)


PolyDrawToolClass = 
	NewClass[
		"Parents"	-> {EditToolClass, DragClass, TapClass},
		"Fields"	-> {
				"drag"	-> "BBool"[True]
			}
	]

PolyDrawToolClass.init[opts : OptionsPattern[PolyDrawToolClass] ] :=
	Module[{attrs},
		o.type	= "PolyDrawTool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* PolyEditToolClass *)

PolyEditToolClass = 
	NewClass[
		"Parents"	-> {EditToolClass, DragClass, TapClass},
		"Fields"	-> {
			"vertex_renderer"	-> "Instance"[GlyphRenderer]  (*Instance(GlyphRenderer, *) 
(*			
	The renderer used to render the vertices of a selected line or
    polygon.
*)			}
	]

PolyEditToolClass.init[opts : OptionsPattern[PolyEditToolClass] ] :=
	Module[{attrs},
		o.type	= "PolyEditTool";

		(* parse options *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ToolbarClass *)

$AllTools = {
		"Pan"
		, "WheelZoom"
		, "BoxZoom"
		, "Save"
		, "Reset"
		, "Crosshair"
		, "Help"
		, "Hover"
		, "Tap"
		, "ZoomIn"
		, "BoxSelect"
		, "LassoSelect"
		, "PolySelect"
		, "Undo"
		, "Redo"
		, "BoxEdit"
		, "PointDraw"
		, "PolyDraw"
		, "PolyEdit"
	}

$DefaultTools = {"Pan", "WheelZoom", "BoxZoom", "Save", "Reset"}

ToolbarClass = 
	NewClass[
		"Parents"	-> {ToolbarBaseClass},
		"Fields"	-> {
			"active_drag" 			-> "default"		(* Either(Auto, Instance(Drag) *)
			, "active_inspect"		-> "default"		(* Either(Auto, Instance(Inspection), Seq(Instance(Inspection)) *)
			, "active_scroll"		-> "default"		(* Either(Auto, Instance(Scroll) *)
			, "active_tap"			-> "default"		(* Either(Auto, Instance(Tap) *)
			}
	]
	
ToolbarClass.init[opts : OptionsPattern[ToolbarClass] ] :=
	Module[{attrs},
		o.type	= "Toolbar";

		(* parse options *)
(*
		tools = o."tools";
		If[tools =!= None,
			tools = Switch[tools,
			 All | "all"			, $DefaultTools,
			 Default | "default"	, $DefaultTools,
			 _						, tools			
			];
			If[!ListQ[tools], tools = {tools}];
			tools = Intersection[tools, $AllTools];
			toollist = {};
			If[toollocation=!=Null,
				toolclasses = ToExpression[#<>"ToolClass"]& /@ tools;
				toollist = BAttribute[# (*, Sequence@@(o.Options) *)]& /@ toolclasses;
				o."tools" = New[BagClass]["content"->toollist];
			]
		];
*)
		(* attribute fields *)
		attrs = o.attributes;
	]
