(* Wolfram Language package *)

(* ::Section:: *)
(* MapOptionsClass *)

MapOptionsClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
			"lat"		-> "BFloat"[ 40.6643]
			, "lng"		-> "BFloat"[-73.9385]
			, "zoom"	-> "BInt"[12]
		}
	]
	
MapOptionsClass.init[opts : OptionsPattern[MapOptionsClass] ] :=
	Module[{attrs},
		o.type = "MapOptions";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* GMapOptionsClass *)

GMapOptionsClass =
	NewClass[
		"Parents"		-> {MapOptionsClass},
		"Fields"		-> {
			"map_type"		-> "Enum"["roadmap", EnumMapType]
(*
    .. _map type: https://developers.google.com/maps/documentation/javascript/reference#MapTypeId
*)
			, "scale_control"	-> "BBool"[False]
(*			
    Whether the Google map should display its distance scale control.
*)
			,  "styles"			-> "JSON"[Null]
(*
    A JSON array of `map styles`_ to use for the GMapPlot. Many example styles can
    `be found here`_.
    .. _map styles: https://developers.google.com/maps/documentation/javascript/reference#MapTypeStyle
    .. _be found here: https://snazzymaps.com
*)
		}
	]
	
GMapOptionsClass.init[opts : OptionsPattern[GMapOptionsClass] ] :=
	Module[{attrs},
		o.type = "GMapOptions";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* MapPlotClass *)

MapPlotClass =
	NewClass[
		"Parents"		-> {PlotModelClass},
		"Fields"		-> {
		}
	]
	
MapPlotClass.init[opts : OptionsPattern[MapPlotClass] ] :=
	Module[{attrs},
		o.type = "MapPlot";

(*

    def __init__(self, *args, **kw):
        from ..models.ranges import Range1d
        for r in ('x_range', 'y_range'):
            if r in kw and not isinstance(kw.get(r), Range1d):
                raise ValueError('Invalid value for %r, MapPlot ranges may only be Range1d, not data ranges' % r)
        super(MapPlot, self).__init__( *args, **kw)

    @error(INCOMPATIBLE_MAP_RANGE_TYPE)
    def _check_incompatible_map_range_type(self):
        from ..models.ranges import Range1d
        if self.x_range is not None and not isinstance(self.x_range, Range1d):
            return "%s.x_range" % str(self)
        if self.y_range is not None and not isinstance(self.y_range, Range1d):
            return "%s.y_range" % str(self)
*)

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* GMapPlotClass *)

GMapPlotClass =
	NewClass[
		"Parents"		-> {MapPlotClass},
		"Fields"		-> {
			"map_options"			-> "Instance"[GMapOptionsClass]
			, "border_fill_color"	-> "BColor"["#ffffff"]
			, "api_key"				-> "BString"[""]
		}
	]
	
GMapPlotClass.init[opts : OptionsPattern[GMapPlotClass] ] :=
	Module[{attrs},
		o.type = "GMapPlot";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* GMapFigureOptionsClass *)

$MAPDEFAULTTOOLS = "pan,wheel_zoom,reset,help"

GMapFigureOptionsClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
			"tools"				-> "Either"[Default, {"BString", "BList"}]	(* Either(String, Seq(Either(String, Instance(Tool))), default=DEFAULT_TOOLS *)
			, "x_minor_ticks"	-> "Either"["auto",{"Auto", "BInt"}]
			, "y_minor_ticks"	-> "Either"["auto",{"Auto", "BInt"}]
			, "x_axis_location" -> "Enum"["below", EnumVerticalLocation]
			, "y_axis_location"	-> "Enum"["left", EnumHorizontalLocation]
			, "x_axis_label"	-> "BString"[""]
			, "y_axis_label"	-> "BString"[""]
			, "active_drag"		-> "Either"["auto", {"Auto", "BString", "Instance"[DragClass]}]
			, "active_inspect"	-> "Either"["auto", {"Auto", "BString", "Instance"[InspectionClass]}] (* Either(Auto, String, Instance(Inspection), Seq(Instance(Inspection)), *)
			, "active_scroll"	-> "Either"["auto", {"Auto", "BString", "Instance"[ScrollClass]}]
 			, "active_tap"		-> "Either"["auto", {"Auto", "BString", "Instance"[TapClass]}]


		}
	]
	
GMapFigureOptionsClass.init[opts : OptionsPattern[GMapFigureOptionsClass] ] :=
	Module[{attrs},
		o.type = "GMapFigureOptions";

		If[o."tools" === Default, o."tools" = $MAPDEFAULTTOOLS];

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* GMapClass *)

GMapClass =
	NewClass[
		"Parents"		-> {GMapPlotClass},
		"Fields"		-> {
				"subtype"	-> "GMap"
				, "_args"	-> {"api_key", "map_options"}
		}
	]
	
GMapClass.init[oopts : OptionsPattern[GMapClass] ] :=
	Module[{attrs, optattrs, opts = oopts},

		If[!IsInstanceQ[o."map_options", GMapOptionsClass], 
			o.$FailedInit = True;
			Return[$Failed]
		];
		
(*
Print[oopts];
Print[OptionValue[GMapClass, oopts, "api_key"]];
*)
		optattrs = Part[opts, All, 1];
		If[MemberQ[optattrs, "plot_width"] && MemberQ[optattrs, "width"], 
			ValueError["Figure called with both \"plot_width\" and \"width\" supplied, supply only one"];
			o.$FailedInit = True;
			Return[$Failed]];
		If[MemberQ[optattrs, "plot_height"] && MemberQ[optattrs, "height"], 
			ValueError["Figure called with both \"plot_height\" and \"height\" supplied, supply only one"];
			o.$FailedInit = True;
			Return[$Failed]];
		If[MemberQ[optattrs, "width"], o."plot_width" = o."width" ];
		If[MemberQ[optattrs, "height"], o."plot_height" = o."plot_height" ];

		o."x_range" = Range1d[];
		o."y_range" = Range1d[];

		(* attribute fields *)
		attrs = o.attributes;
	]
GMapClass.initInstance[opts : OptionsPattern[] ] :=
	Module[{gopts, xf, yf, xt, yt, toolobjs, toolmap},
		o.super.initInstance[opts];

		gopts = GMapFigureOptions[opts];
		
		xf = MercatorTickFormatter["dimension"->"lon"];
		xt = MercatorTicker["dimension"->"lon"];
		o.addlayout[LinearAxis["formatter"->xf, "ticker"->xt], "below"];
		
		yf = MercatorTickFormatter["dimension"->"lat"];
		yt = MercatorTicker["dimension"->"lat"];
		o.addlayout[LinearAxis["formatter"->yf, "ticker"->yt], "left"];

		{toolobjs, toolmap} = processToolsArg[gopts."tools"];
		o.addtools[toolobjs];
		processActiveTools[o."toolbar", toolmap, 
			gopts."active_drag", gopts."active_inspect", 
			gopts."active_scroll", gopts."active_tap"];
	]

(* Marker methods *)

$GMapMarkerTypes = {
	{"asterisk"				, "MarkerAsterisk"		}
	, {"circle"				, "MarkerCircle"		}
	, {"circlecross"		, "MarkerCircleCross"	}
	, {"circlex"			, "MarkerCircleX"		}
	, {"cross"				, "MarkerCross"			}
	, {"diamond"			, "MarkerDiamond"		}
	, {"diamondcross"		, "MarkerDiamondCross"	}	
	, {"invertedtriangle"	, "MarkerInvertedTriangle"}
	, {"square"				, "MarkerSquare"		}
	, {"squarecross"		, "MarkerSquareCross"	}
	, {"squarex"			, "MarkerSquareX"		}
	, {"triangle"			, "MarkerTriangle"		}
	, {"x"					, "MarkerX"				}	
}

ToExpression[StringTemplate[$glyphStringTemplate][<|"class"->"GMapClass", "a"->#[[1]], "b"->#[[2]]|>]]& /@ $GMapMarkerTypes

(* Glyph methods *)

$GMapGlyphTypes = {
	{"annularwedge"			, "AnnularWedge"	}
	, {"annulus"			, "GlyphAnnulus"	}
	, {"arc"				, "Arc"				}
	, {"bezier"				, "Bezier"			}
	, {"ellipse"			, "Ellipse"			}
	, {"hbar"				, "HBar"			}
	, {"image"				, "GlyphImage"		}
	, {"imagergba"			, "ImageRGBA"		}
	, {"imageurl"			, "ImageURL"		}
	, {"line"				, "GlyphLine"		}
	, {"multiline"			, "MultiLine"		}
	, {"oval"				, "Oval"			}
	, {"patch"				, "Patch"			}
	, {"patches"			, "Patches"			}
	, {"quad"				, "Quad"			}
	, {"quadric"			, "Quadric"			}
	, {"ray"				, "Ray"				}
	, {"rect"				, "Rect"			}
	, {"segment"			, "Segment"			}
	, {"text"				, "GlyphText"		}
	, {"vbar"				, "VBar"			}	
	, {"wedge"				, "GlyphWedge"		}
}

ToExpression[StringTemplate[$glyphStringTemplate][<|"class"->"GMapClass", "a"->#[[1]], "b"->#[[2]]|>]]& /@ $GMapGlyphTypes


(* ::Section:: *)
(* gmap *)

Options[gmap] = Join[
	Options[GMapClass], 
	Options[GMapFigureOptionsClass]
	]
gmap[googleapikey_String, mapoptions_, opts : OptionsPattern[]]  /; IsInstanceQ[mapoptions, GMapOptionsClass]:=
	Module[{},
		GMap[googleapikey, mapoptions, opts]
	]

gmap[mapoptions_, opts : OptionsPattern[]]  /; StringQ[$GOOGLEAPI] && IsInstanceQ[mapoptions, GMapOptionsClass]:=
	Module[{},
		GMap[$GOOGLEAPI, mapoptions, opts]
	]

