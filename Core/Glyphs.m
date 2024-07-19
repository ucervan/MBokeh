(* Wolfram Language package *)

(*
	ModelClass
		, GlyphClass
			, XYGlyphClass
				, AnnularWedgeClass
				, GlyphAnnulusClass
				, ArcClass
				, EllipseClass
				, GlyphImageClass
				, ImageRGBAClass
				, ImageURLClass
				, GlyphLineClass
				, OvalClass
				, PatchClass
				, RayClass
				, RectClass
				, StepClass				
				, GlyphTextClass
				, GlyphWedgeClass				
				, MarkerClass
					, MarkerAsteriskClass
					, MarkerCircleClass
					, MarkerCircleCrossClass
					, MarkerCircleXClass
					, MarkerCrossClass
					, MarkerDiamondClass
					, MarkerDiamondCrossClass
					, MarkerHexClass
					, MarkerInvertedTriangleClass
					, MarkerSquareClass
					, MarkerSquareCrossCLass
					, MarkerSquareXClass
					, MarkerTriangleClass
					, MarkerXClass
			, BezierClass
			, HBarClass			
			, HexTileClass			
			, MultiLineClass
			, PatchesClass	
			, QuadClass
			, QuadricClass
			, SegmentClass
			, VBarClass
*)


(* ::Section:: *)
(* GlyphClass *)

(*
@abstract
class Glyph(Model):
    ''' Base class for all glyph models.

*)

GlyphClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {			
			}
	]
	
GlyphClass.init[opts : OptionsPattern[GlyphClass] ] :=
	Module[{attrs},
		o.type	= "Glyph";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* XYGlyphClass *)

(*
@abstract
class XYGlyph(Glyph):
    ''' Base class of glyphs with `x` and `y` coordinate attributes.
*)

XYGlyphClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			}
	]
	
XYGlyphClass.init[opts : OptionsPattern[XYGlyphClass] ] :=
	Module[{attrs},
		o.type	= "XYGlyph";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* AnnularWedgeClass *)

AnnularWedgeClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"				-> 
				{"x", "y", "inner_radius", "outer_radius", "start_angle", "end_angle", "direction"}
			, "x"				-> "NumberSpec"[Null]
			, "y"				-> "NumberSpec"[Null]
			, "inner_radius"	-> "DistanceSpec"[Null]
			, "outer_radius"	-> "DistanceSpec"[Null]
			, "start_angle"		-> "AngleSpec"[Null]
			, "end_angle"		-> "AngleSpec"[Null]
			, "direction"		-> "Enum"["anticlock", EnumDirection]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
AnnularWedgeClass.init[opts : OptionsPattern[AnnularWedgeClass] ] :=
	Module[{attrs},
		o.type	= "AnnularWedge";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphAnnulusClass *)

GlyphAnnulusClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"				-> 
				{"x", "y", "inner_radius", "outer_radius"}
			, "x"				-> "NumberSpec"[Null]
			, "y"				-> "NumberSpec"[Null]
			, "inner_radius"	-> "DistanceSpec"[Null]
			, "outer_radius"	-> "DistanceSpec"[Null]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
GlyphAnnulusClass.init[opts : OptionsPattern[GlyphAnnulusClass] ] :=
	Module[{attrs},
		o.type	= "Annulus";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ArcClass *)

ArcClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"			-> {"x", "y", "radius", "start_angle", "end_angle", "direction"}
			, "x"			-> "NumberSpec"[Null]
			, "y"			-> "NumberSpec"[Null]
			, "radius"		-> "DistanceSpec"[Null]
			, "start_angle"	-> "AngleSpec"[Null]
			, "end_angle"	-> "AngleSpec"[Null]
			, "direction"	-> "Enum"["anticlock", EnumDirection]
			, IncludeProps[LineProps]
			}
	]
	
ArcClass.init[opts : OptionsPattern[ArcClass] ] :=
	Module[{attrs},
		o.type	= "Arc";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* EllipseClass *)

EllipseClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"		-> {"x", "y", "width", "height", "angle"}
			, "x"		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, "width"	-> "DistanceSpec"[Null]
			, "height"	-> "DistanceSpec"[Null]
			, "angle"	-> "AngleSpec"[0.0]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
EllipseClass.init[opts : OptionsPattern[EllipseClass] ] :=
	Module[{attrs},
		o.type	= "Ellipse";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphImageClass *)

GlyphImageClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args" 			-> {"image", "x", "y", "dw", "dh", "global_alpha", "dilate"}
			, "image" 			-> "NumberSpec"[Null]	(* The arrays of RGBA data for the images *)
			, "x"				-> "NumberSpec"[Null] 	(* The x-coordinates to locate the image anchors. *)
			, "y"				-> "NumberSpec"[Null] 	(* The y-coordinates to locate the image anchors. *)
			, "dw"				-> "DistanceSpec"[None]	(* The widths of the plot regions that the images will occupy. *)
			, "dh"				-> "DistanceSpec"[None]	(* The height of the plot region that the image will occupy. *)
			, "global_alpha"	-> "BFloat"[1.0]
			, "dilate"			-> "BBool"[False]
			, "color_mapper" 	-> "Instance"[ColorMapperClass]
			}
	]
	
GlyphImageClass.init[opts : OptionsPattern[GlyphImageClass] ] :=
	Module[{attrs},
		o.type	= "Image";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ImageRGBAClass *)

ImageRGBAClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args" 			-> {"image", "x", "y", "dw", "dh", "global_alpha", "dilate"}
			,"image" 			-> "NumberSpec"[Null]	(* The arrays of RGBA data for the images *)
			,"x"				-> "NumberSpec"[Null] 	(* The x-coordinates to locate the image anchors. *)
			,"y"				-> "NumberSpec"[Null] 	(* The y-coordinates to locate the image anchors. *)
			,"dw"				-> "DistanceSpec"[None]	(* The widths of the plot regions that the images will occupy. *)
			,"dh"				-> "DistanceSpec"[None]	(* The height of the plot region that the image will occupy. *)
			,"global_alpha"		-> "BFloat"[1.0]
			,"dilate"			-> "BBool"[False]
			}
	]
	
ImageRGBAClass.init[opts : OptionsPattern[ImageRGBAClass] ] :=
	Module[{attrs},
		o.type	= "ImageRGBA";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ImageURLClass *)

ImageURLClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args" 			-> {"url", "x", "y", "w", "h", "angle", "global_alpha", "dilate"}
			,"url" 				-> "NumberSpec"[Null, "accept_datetime"->False]		
			,"x"				-> "NumberSpec"[Null] 	(* The x-coordinates to locate the image anchors. *)
			,"y"				-> "NumberSpec"[Null] 	(* The y-coordinates to locate the image anchors. *)
			,"w"				-> "DistanceSpec"[None]	(* The height of the plot region that the image will occupy in data space. *)
			,"h"				-> "DistanceSpec"[None]	(* The height of the plot region that the image will occupy in data space. *)
			,"angle"			-> "AngleSpec"[0]		(* The angles to rotate the images, as measured from the horizontal. *)
			,"global_alpha"		-> "BFloat"[1.0]
			,"dilate"			-> "BBool"[False]
			,"anchor"			-> "Enum"["top_left", EnumAnchor]
			,"retry_attempts"	-> "BInt"[0]
			,"retry_timeout"	-> "BInt"[0]
			}
	]
	
ImageURLClass.init[opts : OptionsPattern[ImageURLClass] ] :=
	Module[{attrs},
		o.type	= "ImageURL";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphLineClass *)

GlyphLineClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
				"_args"		-> {"x", "y"}
				, "x"		-> "NumberSpec"[Null]
				, "y"		-> "NumberSpec"[Null]
				, IncludeProps[LineProps]
			}
	]
	
GlyphLineClass.init[opts : OptionsPattern[GlyphLineClass] ] :=
	Module[{attrs},
		o.type	= "Line";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* OvalClass *)

OvalClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"		-> {"x", "y", "width", "height", "angle"}
			, "x"		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, "width"	-> "DistanceSpec"[Null]
			, "height"	-> "DistanceSpec"[Null]
			, "angle"	-> "AngleSpec"[0.0]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
OvalClass.init[opts : OptionsPattern[OvalClass] ] :=
	Module[{attrs},
		o.type	= "Oval";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* PatchClass *)

PatchClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"		-> {"x", "y"}
			, "x"		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, IncludeProps[ScalarLineProps]
			, IncludeProps[ScalarFillProps]
			}
	]
	
PatchClass.init[opts : OptionsPattern[PatchClass] ] :=
	Module[{attrs},
		o.type	= "Patch";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* RayClass *)

RayClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"		-> {"x", "y", "length", "angle"}
			, "x"		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, "angle"	-> "AngleSpec"[Null]
			, "length"	-> "DistanceSpec"[Null]
			, IncludeProps[LineProps]
			}
	]
	
RayClass.init[opts : OptionsPattern[RayClass] ] :=
	Module[{attrs},
		o.type	= "Ray";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* RectClass *)

RectClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"			-> {"x", "y", "width", "height", "angle", "dilate"}
			, "x"			-> "NumberSpec"[Null]
			, "y"			-> "NumberSpec"[Null]
			, "width"		-> "DistanceSpec"[Null]			
			, "height"		-> "DistanceSpec"[Null]
			, "angle"		-> "AngleSpec"[0.0]
			, "dilate"		-> "BBool"[False]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
RectClass.init[opts : OptionsPattern[RectClass] ] :=
	Module[{attrs},
		o.type	= "Rect";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* StepClass *)

StepClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"		-> {"x", "y"}
			, "x"		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			, "mode"	-> "Enum"["before", EnumStepMode]
			}
	]
	
StepClass.init[opts : OptionsPattern[StepClass] ] :=
	Module[{attrs},
		o.type	= "Step";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphTextClass *)

GlyphTextClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"			-> {"x", "y", "text", "angle", "x_offset", "y_offset"}
			, "x"			-> "NumberSpec"[Null]
			, "y"			-> "NumberSpec"[Null]
			, "text"		-> "StringSpec"["text"]
			, "angle"		-> "AngleSpec"[0]
			, "x_offset"	-> "NumberSpec"[0]
			, "y_offset"	-> "NumberSpec"[0]
			, IncludeProps[TextProps]
			}
	]
	
GlyphTextClass.init[opts : OptionsPattern[TextClass] ] :=
	Module[{attrs},
		o.type	= "Text";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GlyphWedgeClass *)

GlyphWedgeClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
			"_args"			-> {"x", "y", "radius", "start_angle", "end_angle", "direction"}
			, "x"			-> "NumberSpec"[Null]
			, "y"			-> "NumberSpec"[Null]
			, "radius"		-> "DistanceSpec"[Null]
			, "start_angle"	-> "AngleSpec"[Null]
			, "end_angle"	-> "AngleSpec"[Null]
			, "direction"	-> "Enum"["anticlock", EnumDirection]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
GlyphWedgeClass.init[opts : OptionsPattern[GlyphWedgeClass] ] :=
	Module[{attrs},
		o.type	= "Wedge";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerClass *)

(* abstract *)
MarkerClass =
	NewClass[
		"Parents"	-> {XYGlyphClass},
		"Fields"	-> {
    		"_args" 	-> {"x", "y", "size", "angle"}
    		, "x" 		-> "NumberSpec"[Null]
			, "y"		-> "NumberSpec"[Null]
			, "size" 	-> "ScreenDistanceSpec"[4]
			, "angle"	-> "AngleSpec"[0.0]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
MarkerClass.init[opts : OptionsPattern[MarkerClass] ] :=
	Module[{attrs},
		o.type	= "Marker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerAsteriskClass *)

MarkerAsteriskClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerAsteriskClass.init[opts : OptionsPattern[MarkerAsteriskClass] ] :=
	Module[{attrs},
		o.type	= "Asterisk";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerCircleClass *)

MarkerCircleClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			"_args"					-> {"x", "y"}
			, "radius"				-> "DistanceSpec"[None]
			, "radius_dimension"	-> "Enum"["x", {"x","y"}]	(* Enum(enumeration('x', 'y'), *)
(*
    What dimension to measure circle radii along.

    When the data space aspect ratio is not 1-1, then the size of the drawn
    circles depends on what direction is used to measure the "distance" of
    the radius. This property allows that direction to be controlled.
*)			
			}
	]
	
MarkerCircleClass.init[opts : OptionsPattern[MarkerCircleClass] ] :=
	Module[{attrs},
		o.type	= "Circle";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerCircleCrossClass *)

MarkerCircleCrossClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerCircleCrossClass.init[opts : OptionsPattern[MarkerCircleCrossClass] ] :=
	Module[{attrs},
		o.type	= "CircleCross";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerCircleXClass *)

MarkerCircleXClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerCircleXClass.init[opts : OptionsPattern[MarkerCircleXClass] ] :=
	Module[{attrs},
		o.type	= "CircleX";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerCrossClass *)

MarkerCrossClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerCrossClass.init[opts : OptionsPattern[MarkerCrossClass] ] :=
	Module[{attrs},
		o.type	= "Cross";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerDiamondClass *)

MarkerDiamondClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerDiamondClass.init[opts : OptionsPattern[MarkerDiamondClass] ] :=
	Module[{attrs},
		o.type	= "Diamond";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerDiamondCrossClass *)

MarkerDiamondCrossClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerDiamondCrossClass.init[opts : OptionsPattern[MarkerDiamondCrossClass] ] :=
	Module[{attrs},
		o.type	= "DiamondCross";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerHexClass *)

MarkerHexClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerHexClass.init[opts : OptionsPattern[MarkerHexClass] ] :=
	Module[{attrs},
		o.type	= "Hex";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerInvertedTriangleClass *)

MarkerInvertedTriangleClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerInvertedTriangleClass.init[opts : OptionsPattern[MarkerInvertedTriangleClass] ] :=
	Module[{attrs},
		o.type	= "InvertedTriangle";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerSquareClass *)

MarkerSquareClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerSquareClass.init[opts : OptionsPattern[MarkerSquareClass] ] :=
	Module[{attrs},
		o.type	= "Square";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerSquareCrossClass *)

MarkerSquareCrossClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerSquareCrossClass.init[opts : OptionsPattern[MarkerSquareCrossClass] ] :=
	Module[{attrs},
		o.type	= "SquareCross";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerSquareXClass *)

MarkerSquareXClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerSquareXClass.init[opts : OptionsPattern[MarkerSquareXClass] ] :=
	Module[{attrs},
		o.type	= "SquareX";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerTriangleClass *)

MarkerTriangleClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerTriangleClass.init[opts : OptionsPattern[MarkerTriangleClass] ] :=
	Module[{attrs},
		o.type	= "Triangle";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* MarkerXClass *)

MarkerXClass =
	NewClass[
		"Parents"	-> {MarkerClass},
		"Fields"	-> {
			}
	]
	
MarkerXClass.init[opts : OptionsPattern[MarkerXClass] ] :=
	Module[{attrs},
		o.type	= "X";

		(* attribute fields *)
		attrs = o.attributes;
	]


(******************************************************************)


(* ::Subsection:: *)
(* BezierClass *)

BezierClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"x0", "y0", "x1", "y1", "cx0", "cy0", "cx1", "cy1"}
			, "x0"		-> "NumberSpec"[Null]
			, "y0"		-> "NumberSpec"[Null]
			, "x1"		-> "NumberSpec"[Null]
			, "y1"		-> "NumberSpec"[Null]
			, "cx0"		-> "NumberSpec"[Null]
			, "cy0"		-> "NumberSpec"[Null]
			, "cx1"		-> "NumberSpec"[Null]
			, "cy1"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			}
	]
	
BezierClass.init[opts : OptionsPattern[BezierClass] ] :=
	Module[{attrs},
		o.type	= "Bezier";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* HBarClass *)

HBarClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"y", "height", "right", "left"}
			, "y"		-> "NumberSpec"[Null]
			, "height"	-> "NumberSpec"[Null]
			, "left"	-> "NumberSpec"[0]
			, "right"	-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
HBarClass.init[opts : OptionsPattern[HBarClass] ] :=
	Module[{attrs},
		o.type	= "HBar";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* HexTileClass *)

HexTileClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"q", "r"}
			, "size"	-> "BFloat"[1.0]
			, "aspect_scale"	-> "BFloat"[1.0]
			, "r"				-> "NumberSpec"[Null]
			, "q"				-> "NumberSpec"[Null]
			, "scale"			-> "NumberSpec"[1.0]
			, "orientation"		-> "BString"["pointytop"]
			, IncludeProps[LineProps,
    			"line_color"	-> "BColor"[None]
			]
			, IncludeProps[FillProps]
			}
	]
	
HexTileClass.init[opts : OptionsPattern[HexTileClass] ] :=
	Module[{attrs},
		o.type	= "HexTile";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MultiLineClass *)

MultiLineClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
				"_args"		-> {"xs", "ys"}
				, "xs"		-> "NumberSpec"[Null]
				, "ys"		-> "NumberSpec"[Null]
				, IncludeProps[LineProps]
			}
	]
	
MultiLineClass.init[opts : OptionsPattern[MultiLineClass] ] :=
	Module[{attrs},
		o.type	= "MultiLine";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* PatchesClass *)

PatchesClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"xs", "ys"}
			, "xs"		-> "NumberSpec"[Null]
			, "ys"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
PatchesClass.init[opts : OptionsPattern[PatchesClass] ] :=
	Module[{attrs},
		o.type	= "Patches";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* QuadClass *)

QuadClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"left", "right", "top", "bottom"}
			, "left"	-> "NumberSpec"[Null]
			, "right"	-> "NumberSpec"[Null]
			, "bottom"	-> "NumberSpec"[Null]
			, "top"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
QuadClass.init[opts : OptionsPattern[QuadClass] ] :=
	Module[{attrs},
		o.type	= "Quad";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* QuadricClass *)

QuadricClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		->{"x0", "y0", "x1", "y1", "cx", "cy"}
			, "x0"		-> "NumberSpec"[Null]
			, "y0"		-> "NumberSpec"[Null]
			, "x1"		-> "NumberSpec"[Null]
			, "y1"		-> "NumberSpec"[Null]
			, "cx"		-> "NumberSpec"[Null]
			, "cy"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			}
	]
	
QuadricClass.init[opts : OptionsPattern[QuadricClass] ] :=
	Module[{attrs},
		o.type	= "Quadric";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* SegmentClass *)

SegmentClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
			"_args"		-> {"x0", "y0", "x1", "y1"}
			, "x0" 		-> "NumberSpec"[Null]
			, "y0"		-> "NumberSpec"[Null]
			, "x1"		-> "NumberSpec"[Null]
			, "y1"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			}
	]
	
SegmentClass.init[opts : OptionsPattern[SegmentClass] ] :=
	Module[{attrs},
		o.type	= "Segment";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* VBarClass *)

(*
class VBar(Glyph):
    ''' Render vertical bars, given a center coordinate, width and (top, bottom) coordinates.
*)

VBarClass =
	NewClass[
		"Parents"	-> {GlyphClass},
		"Fields"	-> {
(*
    # a canonical order for positional args that can be used for any
    # functions derived from this class
 *)
 			"_args" 	->  {"x", "width", "top", "bottom"}
    		, "x"		-> "NumberSpec"[Null]
			, "width"	-> "NumberSpec"[Null]
			, "bottom"	-> "NumberSpec"[0]
			, "top"		-> "NumberSpec"[Null]
			, IncludeProps[LineProps]
			, IncludeProps[FillProps]
			}
	]
	
VBarClass.init[opts : OptionsPattern[VBarClass] ] :=
	Module[{attrs},
		o.type	= "VBar";

		(* attribute fields *)
		attrs = o.attributes;
	]
