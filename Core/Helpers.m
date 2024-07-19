(* Wolfram Language package *)


(* ::Section:: *)
(* Helpers  *)

getRange[None | Null]:= DataRange1d[]
getRange[r_] /; IsInstanceQ[r, RangeClass] := r
getRange[rangeinput_List] :=
	Module[{types},
		types = Union[Head/@rangeinput];
		If[types == {String}, Return[FactorRange["factors"->rangeinput]]];
		If[Length[rangeinput]==2,
			Return[ Range1d["start"->rangeinput[[1]], "end"->rangeinput[[2]]] ]
		];
		ValueError["Unrecognized range input: `1`", {ToString[rangeinput]}];
	]
getRange[r_, ___] := ValueError["Unrecognized range input: `1`", {ToString[r]}] 
	
getScale[rangeinput_, axistype_] :=
	Module[{},
		Which[
			IsInstanceQ[rangeinput, {DataRange1dClass, Range1dClass}] && 
				MemberQ[{"linear", "datetime", "mercator", "auto", None, Null}, axistype],
				LinearScale[],
			IsInstanceQ[rangeinput, {DataRange1dClass, Range1dClass}] && axistype === "log",
				LogScale[],
			IsInstanceQ[rangeinput, FactorRangeClass],
				CategoricalScale[],
			True,
				ValueError["Unable to determine proper scale for: `1`", {ToString[rangeinput]}];
				None
		]
	]
	
getAxisClass[axistype_, rangeinput_, dim_] := 
	Module[{},
		Switch[axistype,
			None		, {None, {}},
			"linear"	, {LinearAxisClass, {}},
			"log"		, {LogAxisClass, {}},
			"datetime"	, {DatetimeAxisClass, {}},
			"mercator"	, {MercatorAxisClass, {"dimension"-> If[dim === 0, "lon", "lat"]}},
			"auto",
				Switch[rangeinput.type,
					"FactorRange"	,	{CategoricalAxisClass, {}},
					"Range1d"		,	If[NumberQ[rangeinput.start], (* TODO: Validate datetime *)
											{LinearAxisClass, {}}, 
											{DatetimeAxisClass, {}}],
					_				,	{LinearAxisClass, {}}
				],
			_, {LinearAxisClass, {}}
		]
	]

knownTools[tool_String] := 
	Switch[tool,
	"pan" | "Pan"					, PanTool["dimensions"->"both"],
    "xpan" | "XPan"					, PanTool["dimensions"->"width"],
    "ypan" | "YPan"					, PanTool["dimensions"->"height"],
    "xwheel_pan" | "XWheelPan"		, WheelPanTool["dimension"->"width"],
    "ywheel_pan" | "YWheelPan"		, WheelPanTool["dimension"->"height"],
    "wheel_zoom" | "WheelZoom"		, WheelZoomTool["dimensions"->"both"],
    "xwheel_zoom" | "XWheelZoom"	, WheelZoomTool["dimensions"->"width"],
    "ywheel_zoom" | "YWheelZoom"	, WheelZoomTool["dimensions"->"height"],
    "zoom_in" | "ZoomIn"			, ZoomInTool["dimensions"->"both"],
    "xzoom_in" | "XZoomIn"			, ZoomInTool["dimensions"->"width"],
    "yzoom_in" | "YZoomIn"			, ZoomInTool["dimensions"->"height"],
    "zoom_out" | "ZoomOut"			, ZoomOutTool["dimensions"->"both"],
    "xzoom_out" | "XZoomOut"		, ZoomOutTool["dimensions"->"width"],
    "yzoom_out" | "YZoomOut"		, ZoomOutTool["dimensions"->"height"],
    "click" | "Click"				, TapTool["behavior"->"inspect"],
    "tap" | "Tap"					, TapTool[],
    "crosshair" | "Crosshair"		, CrosshairTool[],
    "box_select" | "BoxSelect"		, BoxSelectTool[],
    "xbox_select" | "XBoxSelect"	, BoxSelectTool["dimensions"->"width"],
    "ybox_select" | "YBoxSelect"	, BoxSelectTool["dimensions"->"height"],
    "poly_select" | "PolySelect"	, PolySelectTool[],
    "lasso_select" | "LassoSelect"	, LassoSelectTool[],
    "box_zoom" | "BoxZoom"			, BoxZoomTool["dimensions"->"both"],
    "xbox_zoom" | "XBoxZoom"		, BoxZoomTool["dimensions"->"width"],
    "ybox_zoom" | "YBoxZoom"		, BoxZoomTool["dimensions"->"height"],
    "hover" | "Hover"				, HoverTool["tooltips"->{
        	{"index", "$index"},
     	   	{"data (x, y)", "($x, $y)"},
       	 	{"screen (x, y)", "($sx, $sy)"}
    		}],
    "save" | "previewsave" | "Save"	, SaveTool[],
    "undo" | "Undo"					, UndoTool[],
    "redo" | "Redo"					, RedoTool[],
    "reset" | "Reset"				, ResetTool[],
    "help" | "Help"					, HelpTool[],
    "box_edit" | "BoxEdit"			, BoxEditTool[],
    "point_draw" | "PointDraw"		, PointDrawTool[],
    "poly_draw" | "PolyDraw"		, PolyDrawTool[],
    "poly_edit" |"PolyEdit"			, PolyEditTool[],
    _, None
	]
toolFromString[tool_String] := 
	Module[{toolfn},
		toolfn = knownTools[tool];
		If[toolfn === None,
			ValueError["unexpected tool name `1`", tool];
			Return[$Failed]		
		];
		{tool->toolfn}
	]

getNumMinorTicks[axisclass_		, nminticks_Integer] /; nminticks > 1 := nminticks
getNumMinorTicks[LogAxisClass	, "auto"] := 10
getNumMinorTicks[axisclass_		, "auto"] := 5
getNumMinorTicks[axisclass_		, nminticks_] := 0

	
SetAttributes[processAxisAndGrid, HoldFirst]
processAxisAndGrid[plot_, axistype_, axislocation_, minorticks_, axislabel_, rng_, dim_] :=
	Module[{axiscls, axopts, axis, grid},

		{axiscls, axopts} = getAxisClass[axistype, rng, dim];		
		If[axiscls === None, Return[]];
(*
		If[axiscls === LogAxisClass,
			If[dim ===0, (* then *)
				plot."x_scale" = New[LogScaleClass][], (* else *)
				Assert[dim === 1];
				plot."y_scale" = New[LogScaleClass][]
			]
		];
*)
		axis = New[axiscls]["plot"->If[StringQ[axislocation], plot, None], Sequence@@axopts];
		Quiet@(axis.initInstance[ Sequence@@axopts]);
		
		If[IsInstanceQ[axis."ticker", ContinuousTickerClass], 
			axis."ticker"."num_minor_ticks" = getNumMinorTicks[axiscls, minorticks]
		];

		If[StringQ[axislabel],
			axis."axis_label" = axislabel;
		];
		
		grid = GridModel["plot"->plot, "dimension"->dim, "ticker"->axis."ticker"];
		
		If[HasAttrQ[plot, axislocation],
			plot.axislocation.stuff[axis]
		];
	]
	
processToolsArg[tools_List] :=
	Module[{strtools, fntools, alltools, classes, toolsmap},
		strtools = StringTrim /@ Join @@ (StringSplit[#, ","] & /@ Cases[tools, _String]);		
		toolsmap = Cases[Flatten[toolFromString/@ strtools,1], _Rule];
		strtools = toolsmap[[All,2]];
		strtools = Select[strtools, IsInstanceQ[#, ToolClass] &]; 

		fntools = Select[tools, IsInstanceQ[#, ToolClass] &];
		alltools = Join[fntools, strtools];

		classes = Union[(#.type[])& /@ alltools];
		If[Length[classes]=!=Length[alltools],
			ValueWarning["tools are repeated"]
		];
		{alltools, Association[toolsmap]}
	]
processToolsArg[tools_String] := processToolsArg[{tools}]
processToolsArg[tool_] /; IsInstanceQ[tool, ToolClass] := {tool, <||>}

SetAttributes[processActiveTools, HoldFirst]
processActiveTools[toolbar_, toolmap_Association, 
	activedrag_, activeinspect_, activescroll_, activetap_] :=
	Module[{},
		Which[
			MemberQ[{"auto", None}, activedrag] || IsInstanceQ[activedrag, ToolClass],
				toolbar."active_drag" = activedrag,
			KeyExistsQ[toolmap, activedrag],
				toolbar."active_drag" = toolmap[activedrag],
			True,
				ValueError["Got unknown `1` for \"active_drag\", which was ot a string supplied in tools argument", 
					activedrag];
				Return[$Failed]
		];
		Which[
			MemberQ[{"auto", None}, activeinspect] || IsInstanceQ[activeinspect, ToolClass],
				toolbar."active_inspect" = activeinspect,
			KeyExistsQ[toolmap, activeinspect],
				toolbar."active_inspect" = toolmap[activeinspect],
			True,
				ValueError["Got unknown `1` for \"active_inspect\", which was ot a string supplied in tools argument", 
					activeinspect];
				Return[$Failed]
		];
		Which[
			MemberQ[{"auto", None}, activescroll] || IsInstanceQ[activescroll, ToolClass],
				toolbar."active_scroll" = activescroll,
			KeyExistsQ[toolmap, activescroll],
				toolbar."active_scroll" = toolmap[activescroll],
			True,
				ValueError["Got unknown `1` for \"active_scroll\", which was ot a string supplied in tools argument", 
					activescroll];
				Return[$Failed]
		];
		Which[
			MemberQ[{"auto", None}, activetap] || IsInstanceQ[activetap, ToolClass],
				toolbar."active_tap" = activetap,
			KeyExistsQ[toolmap, activetap],
				toolbar."active_tap" = toolmap[activetap],
			True,
				ValueError["Got unknown `1` for \"active_tap\", which was ot a string supplied in tools argument", 
					activetap];
				Return[$Failed]
		];
	]
	
	
(* ::Section:: *)
(* glyphFunction  *)

Clear[getLegendItemLabel]
SetAttributes[getLegendItemLabel, HoldFirst]
getLegendItemLabel[args_] :=
	Module[{legend, source,legenditemlabel},
		legend = If[KeyExistsQ[args, "legend"], args["legend"], None ];
		source = If[KeyExistsQ[args, "source"], args["source"], None ];
		legenditemlabel = None;
		If[legend=!=None,
			If[StringQ[legend], (* then *)
				legenditemlabel = value[legend];		
				(*TODO: Process "value"() and "field"() if a source is given *)
				If[IsInstanceQ[source, ColumnDataSourceClass] && MemberQ[source.columnnames[], legend], 
					legenditemlabel = field[legend]		
				],				
				legenditemlabel = legend				
			]
		];
		legenditemlabel
	]

$RenderArgs = {"name", "x_range_name", "y_range_name", "level", "view", "visible", "muted"}
	
Clear[popRenderArgs]
SetAttributes[popRenderArgs, HoldFirst]
popRenderArgs[args_] :=
	Module[{result = <||>},
		Scan[If[!MissingQ[args[#]], result[#] = args[#]]&, $RenderArgs];
		result["data_source"] = If[!MissingQ[args["source"]], args["source"], ColumnDataSource[]];
		result
	]


$defColors = {
        "#1f77b4",
        "#ff7f0e", "#ffbb78",
        "#2ca02c", "#98df8a",
        "#d62728", "#ff9896",
        "#9467bd", "#c5b0d5",
        "#8c564b", "#c49c94",
        "#e377c2", "#f7b6d2",
        "#7f7f7f",
        "#bcbd22", "#dbdb8d",
        "#17becf", "#9edae5"
}

Clear[getDefaultColor]
getDefaultColor[None] := First@$defColors
getDefaultColor[plot_] :=
	Module[{},
		(* TODO: Check length of plot.renderers and find "GlyphRenderer" instances *)
		First[$defColors]		
	]

Clear[popColorAndAlpha]
SetAttributes[popColorAndAlpha, HoldFirst]
popColorAndAlpha[args_, class_, prefix_:"", defalpha_:1.0] :=
	Module[{result = <||>, color, alpha},
		color = If[!MissingQ[args[prefix<>"color"]], args[prefix<>"color"], getDefaultColor[None]];		
		Scan[If[!MissingQ[$BokehClassAttributesTemplates.class.#],
				result[#] = If[!MissingQ[args[prefix<>#]], args[prefix<>#], color]			
			]&, {"fill_color", "line_color"}];
			
		If[!MissingQ[$BokehClassAttributesTemplates.class."text_color"],
			result["text_color"] = If[!MissingQ[args[prefix<>"text_color"]],args[prefix<>"text_color"], "black" ];
		];

		alpha = If[!MissingQ[args[prefix<>"alpha"]], args[prefix<>"alpha"], defalpha];
		Scan[If[!MissingQ[$BokehClassAttributesTemplates.class.#],
				result[#] = If[!MissingQ[args[prefix<>#]], args[prefix<>#], alpha]
			]&, {"fill_alpha", "line_alpha", "text_alpha"}];
				
		result		
	]

$GLYPHSOURCEMSG = "

Error in:  `1`->`2`

Supplying a user-defined data source AND iterable values to glyph methods is
not possibe. Either:

Pass all data directly as literals:

    p.circe[\"x\"->a_list, \"y\"->an_array, ...]

Or, put all data in a ColumnDataSource and pass column names:

    source = ColumnDataSource[\"data\"-><|\"x\"->a_list, \"y\"->an_array|>]
    p.circe[\"x\"->\"x\", \"y\"->\"y\", \"source\"->source, ...]
"

Clear[processSequenceLiterals]
SetAttributes[processSequenceLiterals, HoldAll]
processSequenceLiterals[args_, class_, source_, isusersource_] :=
	Module[{keys = Keys[args], dataspecs, speckeys, val},
(*
		dataspecs = getArgsSpecs[class][[All,"type"]];
*)
		dataspecs = Part[$BokehClassAttributesTemplates.class, 1][[All, 4]];
		speckeys = Keys[dataspecs];
		Scan[
			(
			val = args[#];
			If[NumericQ[val] || NumberQ[val] || StringQ[val] ||
				AssociationQ[val] ||
				!MemberQ[speckeys, #] || 
				MatchQ[val,
					(None | Null | True | False | _value | _field | _expr)
					] || (* Let Properties validation expand these *)
				(dataspecs[#] === "ColorSpec" && AssociationQ[val])  (* a tuple is an association with integer keys *)
(*				|| (ArrayQ[val] && ArrayDepth[val]=!=1) *)
				,
				(* Then *)
					None
				, (* else *)
				Which[
					(packedArrayQ[val] && ArrayDepth[val]=!=1),
						RuntimeError["Columns need to be 1D (`1` is not)", {val}],
					isusersource,
						RuntimeError[$GLYPHSOURCEMSG, {#, val}],
					True, 
						source.add[val, #];
						args[#] = #
				]
			])&, keys]
	]
	
Clear[makeGlyph]
makeGlyph[_, _, None] := None
makeGlyph[class_, okws_, extra_] :=
	Module[{kws = okws},
	kws = Join[kws, extra];
	New[class][Normal[kws]]
	]
	
Clear[updateLegend]
SetAttributes[updateLegend, HoldAll]
updateLegend[plot_, legenditemlabel_, renderer_]:=
	Module[{legends, legend, added, newitem},
		(* TODO: Implement me *)
		legends = plot.select[LegendClass];
		Which[
			legends.len[] === 0,
				legend = Legend[];
				plot.addlayout[legend],
			legends.len[] === 1,
				legend = legends.part[1],
			True,
				RuntimeError["Plot `1` configured with more than one legend renderer", plot.id]
		];
		
		added = False;
		Scan[
			If[#.label === legenditemlabel,
				Which[
					MatchQ[#.label, value[_]], 
						#.renderers.stuff[renderer]; 
						added = True,
					MatchQ[#.label, field[_]] && 
						renderer."data_source".id === (#.renderers.part[1])."data_source".id,
						#.renderers.stuff[renderer];
						added = True
				]
			]&
		, legend.items.all[] ];

		If[!added,
			newitem = LegendItem["label"->legenditemlabel, "renderers"-> renderer];
			legend.items.stuff[newitem]
		];
	]

getArgs[x__, y : OptionsPattern[]] :=
 Module[{args = {x}, opts = {y}},
  If[MatchQ[args, {_Rule}], opts = Prepend[opts, x]; args = {}];
  If[opts === {} && !FreeQ[args,Rule], opts = args; args = {}];
  Association[{"args" -> args, "opts" -> opts}]
  ]
ValidatePropsFunc[props_List] := Part[props, 1]
ValidatePropsFunc[class_, field_String] := ValidatePropsFunc[$BokehClassAttributesTemplates.class.field]
PropsDefault[props_List] := Part[props, 2]
PropsDefault[class_, field_String] := PropsDefault[$BokehClassAttributesTemplates.class.field]
PropsOptions[props_List] := Part[props, 3]
PropsOptions[class_, field_String] := PropsOptions[$BokehClassAttributesTemplates.class.field]
getArgType[(p_String)[___]] := ToExpression[p]
getArgType[_] := None
getArgsSpecs[class_] :=
 Module[{rules, args},
  	args = PropsDefault[class, "_args"];
  	rules = If[ListQ[args], Flatten[Last@Reap[Scan[        		
        Sow[# -> <|"default" -> PropsDefault[class, #], 
            "type" -> getArgType@(# /. Options[class])|>] &
        	, args]], 1], {}];
   	Association@rules
  ]

SetAttributes[glyphFunction, HoldFirst]
glyphFunction[this_, class_, x_, y_] :=
	Module[{argspec, classargs, defargs, argnames, arglen, defassoc, glyphopts, func, kwargs},

		func[] := Module[
			{argkeys, legenditemlabel, isusersource, renderkws, source,
			glyphca, nsglyphca, sglyphca, hglyphca, mglyphca, argcases,
			glyph, nsglyph, sglyph, hglyph, mglyph, glyphrenderer},


			legenditemlabel = getLegendItemLabel[kwargs];
			isusersource = !MissingQ[kwargs["source"]];
			renderkws = popRenderArgs[kwargs];
			source = renderkws["data_source"];
			
			If[KeyExistsQ[kwargs, "alpha"] && 
				MemberQ[{GlyphImageClass, ImageRGBAClass, ImageURLClass}, class],
				kwargs["global_alpha"] = kwargs["alpha"]
			];

			If[!IsInstanceQ[source, ColumnarDataSourceClass],
				source = ColumnDataSource[source]; (* TODO: implement test for valid conversion *)
				renderkws["data_source"] = source
			];

			(* handle the main glyph, need to process literals *)			
			glyphca = popColorAndAlpha[kwargs, class];

			processSequenceLiterals[kwargs, class, source, isusersource];
			processSequenceLiterals[glyphca, class, source, isusersource];

			nsglyphca = popColorAndAlpha[kwargs, class, "nonselection_", 0.1];
			
			argkeys = Keys[kwargs];
			argcases = Flatten@StringCases[argkeys, StartOfString~~"selection_"~~__];
			sglyphca = If[Length[argcases]>0, popColorAndAlpha[kwargs, class, "selection_"], None];
			argcases = Flatten@StringCases[argkeys, StartOfString~~"hover_"~~__];
			hglyphca = If[Length[argcases]>0, popColorAndAlpha[kwargs, class, "hover_"], None];
			argcases = Flatten@StringCases[argkeys, StartOfString~~"muted_"~~__];
			mglyphca = If[Length[argcases]>0, popColorAndAlpha[kwargs, class, "muted_"], None];

			glyph 	= makeGlyph[class, kwargs, glyphca];
			nsglyph = makeGlyph[class, kwargs, nsglyphca];
			sglyph	= makeGlyph[class, kwargs, sglyphca];
			hglyph	= makeGlyph[class, kwargs, hglyphca];
			mglyph	= makeGlyph[class, kwargs, mglyphca];

			glyphrenderer = GlyphRenderer[
							"glyph"					-> glyph,
							"nonselection_glyph"	-> nsglyph,
							"selection_glyph"		-> sglyph,
							"hover_glyph"			-> hglyph,
							"muted_glyph"			-> mglyph,
							Sequence@@(Normal[renderkws])
							];

			If[legenditemlabel=!=None,
				updateLegend[this, legenditemlabel, glyphrenderer]
				];
(*				
			tools = this.select[BoxSelectToolClass];
			If[tools.len[]>0,
				Scan[ ( (#."renderers").stuff[glyphrenderer])&, tools."data"]
			];
*)			
			this."renderers".stuff[glyphrenderer];

			glyphrenderer
		];
		
	argspec = getArgs[Sequence @@ x, Sequence @@ y];
	classargs = getArgsSpecs[class];
	argnames = Keys[classargs];
	
	defassoc = Part[classargs, All, "default"];
	defargs = Values@defassoc;
	arglen = Length[argspec["args"]];

	glyphopts = Table[Rule[Part[argnames, i], If[i <= arglen, Part[argspec["args"], i], Part[defargs, i]]], 
		{i, 1, Length[argnames]} ];

	kwargs = Join[Association[glyphopts], Association[argspec["opts"]]];

	func[]
  ]

stack0[stackers_List, spec0_String, spec1_String, kw_Association]  :=
	Module[{lengths, s0, s1, okw, len, i, d, val},
		Scan[If[KeyExistsQ[kw, #],
			ValueError["Stack property `1` cannot appear in keyword args", #];
			Return[{}]
		]&, {spec0, spec1}];
		
		len = Length[stackers];
		lengths = Union@Values[If[ListQ[#], Length[#], Sequence @@ {}] & /@ kw];
		If[Length[lengths]>0,
			If[Length[lengths]=!=1,
				ValueError["Keyword argument sequences for broadcasting must all be the same lengths. Got lengths: `1`", lengths];
				Return[{}]
			];
			If[lengths[[1]] =!= len,
				ValueError["Keyword argument sequences for broadcasting must be the same length as stackers"];
				Return[{}]
			]
		];
		
		s0 = {}; s1= {};
		okw = Bag[];
		
		For[i=1, i<=len , i++,
			val = stackers[[i]];
			d = <||>;
			s0 = s1;
			s1 = Append[s1, val];
			
			d[spec0] = stack[s0];
			d[spec1] = stack[s1];
			
			MapThread[(d[#1] = If[ListQ[#2], Part[#2,i], #2])&, {Keys[kw], Values[kw]}];

			okw.stuff[d, False]
		];
		Normal[#]& /@ (okw.all[])
	]

overlap0[stackers_List, spec0_String, spec1_String, kw_Association]  :=
	Module[{lengths, okw, len, i, d, val},
		Scan[If[KeyExistsQ[kw, #],			
			ValueError["Stack property `1` cannot appear in keyword args", #];
			Return[{}]
		]&, {spec0, spec1}];
		
		len = Length[stackers];
		lengths = Union@Values[If[ListQ[#], Length[#], Sequence @@ {}] & /@ kw];
		If[Length[lengths]>0,
			If[Length[lengths]=!=1,
				ValueError["Keyword argument sequences for broadcasting must all be the same lengths. Got lengths: `1`", lengths];
				Return[{}]
			];
			If[lengths[[1]] =!= len,
				ValueError["Keyword argument sequences for broadcasting must be the same length as stackers"];
				Return[{}]
			]
		];

		okw = Bag[];
		For[i=1, i<=len , i++,
			val = stackers[[i]];
			d = <||>;
			d[spec1] = field[val];
			MapThread[(d[#1] = If[ListQ[#2], Part[#2,i], #2])&, {Keys[kw], Values[kw]}];
			okw.stuff[d, False]
		];
		Normal[#]& /@ (okw.all[])
	]
  