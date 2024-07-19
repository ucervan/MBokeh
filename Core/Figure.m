(* Wolfram Language package *)

(* ::Section:: *)
(* FigureOptionsClass *)

$DEFAULTTOOLS = "pan,wheel_zoom,box_zoom,save,reset,help"

FigureOptionsClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
			"tools"		-> "BList"[Default]
			, "x_range"		-> "Any"[Null]
			, "y_range"		-> "Any"[Null]
			, "x_minor_ticks"	-> "Either"["auto", {"Auto", "BInt"}]
			, "y_minor_ticks"	-> "Either"["auto", {"Auto", "BInt"}]
			, "x_axis_location"	-> "Enum"["below", EnumVerticalLocation]
			, "y_axis_location"	-> "Enum"["left", EnumHorizontalLocation]
			, "x_axis_label"	-> "BString"[""]
			, "y_axis_label"	-> "BString"[""]
			, "active_drag"		-> "Either"["auto", {"Auto", "BString", "Instance"[DragClass]}]
			, "active_inspect"	-> "Either"["auto", {"Auto", "BString", "Instance"[InspectionClass]}]
			, "active_scroll"	-> "Either"["auto", {"Auto", "BString", "Instance"[ScrollClass]}]
 			, "active_tap"		-> "Either"["auto", {"Auto", "BString", "Instance"[TapClass]}]
			, "x_axis_type"		-> "Either"["auto", {"Auto", "Enum"[{"linear", "log", "datetime", "mercator"}]}]
			, "y_axis_type"		-> "Either"["auto", {"Auto", "Enum"[{"linear", "log", "datetime", "mercator"}]}]
		}
	]
	
FigureOptionsClass.init[opts___] :=
	Module[{attrs},
		o.type = "FigureOptions";

		If[o."tools"=== Default, o."tools" = $DEFAULTTOOLS];

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* FigureClass *)

FigureClass = 
	NewClass[
		"Parents"	-> {PlotModelClass},
		"Fields"	-> {
				"subtype"			-> "Figure"
				, "output_filename" -> $BokehOutputFileName
			}
	]	
FigureClass.init[opts : OptionsPattern[FigureClass] ] :=
	Module[{attrs},

		(* attribute fields *)
		attrs = o.attributes;
	]
FigureClass.initInstance[opts : OptionsPattern[] ] :=
	Module[{fopts, toolobjs, toolmap},

		o.super.initInstance[opts];

		fopts = FigureOptions[opts];
		
		o."x_range" = getRange[fopts."x_range"];
		o."y_range" = getRange[fopts."y_range"];
		
		o."x_scale" = getScale[o."x_range", fopts."x_axis_type"];
		o."y_scale" = getScale[o."y_range", fopts."y_axis_type"];
		
		processAxisAndGrid[o,
			fopts."x_axis_type", fopts."x_axis_location", fopts."x_minor_ticks",
			fopts."x_axis_label", o."x_range", 0];
		processAxisAndGrid[o,
			fopts."y_axis_type", fopts."y_axis_location", fopts."y_minor_ticks",
			fopts."y_axis_label", o."y_range", 1];

		{toolobjs, toolmap} = processToolsArg[fopts."tools"];
		o.addtools[toolobjs];
		processActiveTools[o."toolbar", toolmap, 
			fopts."active_drag", fopts."active_inspect", 
			fopts."active_scroll", fopts."active_tap"];
	]

$glyphStringTemplate = "
`class`.`a`[x__, y : OptionsPattern[]]	:= glyphFunction[o, `b`Class , {x}, {y}]
`class`.`a`[] 							:= ValueError[\"`a` needs arguments ...\"]
"

(* Marker methods *)

$MarkerTypes = {
	{"asterisk"				, "MarkerAsterisk"		}
	, {"circle"				, "MarkerCircle"		}
	, {"circlecross"		, "MarkerCircleCross"	}
	, {"circlex"			, "MarkerCircleX"		}
	, {"cross"				, "MarkerCross"			}
	, {"diamond"			, "MarkerDiamond"		}
	, {"diamondcross"		, "MarkerDiamondCross"	}	
	, {"hex"				, "MarkerHex"			}
	, {"invertedtriangle"	, "MarkerInvertedTriangle"}
	, {"square"				, "MarkerSquare"		}
	, {"squarecross"		, "MarkerSquareCross"	}
	, {"squarex"			, "MarkerSquareX"		}
	, {"triangle"			, "MarkerTriangle"		}
	, {"x"					, "MarkerX"				}	
}

ToExpression[StringTemplate[$glyphStringTemplate][<|"class"->"FigureClass", "a"->#[[1]], "b"->#[[2]]|>]]& /@ $MarkerTypes

(* Glyph methods *)

$GlyphTypes = {
	{"annularwedge"			, "AnnularWedge"	}
	, {"annulus"			, "GlyphAnnulus"	}
	, {"arc"				, "Arc"				}
	, {"bezier"				, "Bezier"			}
	, {"ellipse"			, "Ellipse"			}
	, {"hbar"				, "HBar"			}
	, {"hextile"			, "HexTile"			}
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
	, {"step"				, "Step"			}
	, {"text"				, "GlyphText"		}
	, {"vbar"				, "VBar"			}	
	, {"wedge"				, "GlyphWedge"		}
}

ToExpression[StringTemplate[$glyphStringTemplate][<|"class"->"FigureClass", "a"->#[[1]], "b"->#[[2]]|>]]& /@ $GlyphTypes

(* chart methods *)

$ScatterConversions = <|
	"*"		-> "asterisk"
	, "+"	-> "cross"
	, "o"	-> "circle"
	, "ox"	-> "circlex"
	, "o+"	-> "circlecross"
|>

FigureClass.scatter[opts___] :=
	Module[{markertype, func},
			markertype = "marker" /. Cases[{opts},(_Rule | _RuleDelayed)] /. {"marker"->"circle"};
			If[KeyExistsQ[$ScatterConversions, markertype],
				markertype = $ScatterConversions[markertype], (* else *)
				If[!MemberQ[$MarkerTypes[[All,1]], markertype],
					ValueError["`1` is not a recognized scatter marker.", markertype];
					Return[$Failed]
				]
			];
		func = Hold[o.ToExpression[markertype][opts]];
		ReleaseHold[func]
	]
FigureClass.hbarstack[stackers_, opts : OptionsPattern[] ] :=
		(o.hbar[Sequence@@#])& /@ stack0[stackers, "left", "right", Association[opts]]
FigureClass.vbarstack[stackers_, opts : OptionsPattern[] ] :=
		(o.vbar[Sequence@@#])& /@ stack0[stackers, "bottom", "top", Association[opts]]

FigureClass.hbaroverlap[stackers_, opts : OptionsPattern[] ] :=
		(o.hbar[Sequence@@#])& /@ overlap0[stackers, "left", "right", Association[opts]]
FigureClass.vbaroverlap[stackers_, opts : OptionsPattern[] ] :=
		(o.vbar[Sequence@@#])& /@ overlap0[stackers, "bottom", "top", Association[opts]]


FigureClass.show["HTML", version_:MBokeh`Private`$BokehVersion]		:= show[o, "HTML"	, version]
FigureClass.show["Widget", version_:MBokeh`Private`$BokehVersion] 	:= show[o, "Widget"	, version]
FigureClass.show[version_:MBokeh`Private`$BokehVersion, ___] 		:= show[o, "WEB"	, version]

	
(* figure *)

Options[figure] = Join[
	Options[FigureClass],
	Options[FigureOptionsClass]	
]
figure[opts : OptionsPattern[]] :=
	Module[{},
		Figure[opts]
	]

