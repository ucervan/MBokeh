(* Wolfram Language package *)


(* ::Section:: *)
(* Primitive Props *)

Clear[IncludeProps]
IncludeProps[props_List, opts : OptionsPattern[] ] 					:= IncludeProps[{props, ""}, opts]
IncludeProps[props_List, "", opts : OptionsPattern[] ] 				:= IncludeProps[{props, ""}, opts]
IncludeProps[props_List, prefix_String, opts : OptionsPattern[]]	:= IncludeProps[{props, prefix <> "_"}, opts]
IncludeProps[{props_List, prefix_String}, opts : OptionsPattern[] ] := Sequence @@ 
		Join[{opts}, props /. {Rule[p_String, val_] :> Rule[prefix <> p, val]}]
   
LineProps = {
    "line_color"			-> "ColorSpec"["black"]
    , "line_width"			-> "NumberSpec"[1]
	, "line_alpha"			-> "NumberSpec"[1.0]
	, "line_join"			-> "miter"					(* Enum(LineJoin *)
	, "line_cap"			-> "butt"					(* Enum(LineCap *)
	, "line_dash"			-> "DashPattern"["solid"]
	, "line_dash_offset"	-> "BInt"[0]
	}

ScalarLineProps = {
    "line_color"			-> "BColor"["black"]
    , "line_width"			-> "BFloat"[1]
	, "line_alpha"			-> "BPercent"[1.0]
	, "line_join"			-> "miter"					(* Enum(LineJoin *)
	, "line_cap"			-> "butt"					(* Enum(LineCap *)
	, "line_dash"			-> "DashPattern"["solid"]
	, "line_dash_offset"	-> "BInt"[0]
	}

FillProps = {
    "fill_color"			-> "ColorSpec"["gray"]
    , "fill_alpha"			-> "NumberSpec"[1.0]
    }

ScalarFillProps = {
    "fill_color"			-> "BColor"["gray"]
    , "fill_alpha"			-> "BPercent"[1.0]
    }
    
TextProps = {
	"text_font"				-> "BString"["helvetica"]
	, "text_font_size"		-> "FontSizeSpec"["12pt"]	(* FontSizeSpec(value("12pt")) *)
	, "text_font_style"		-> "normal"					(* Enum(FontStyle *)
	, "text_color"			-> "ColorSpec"["#444444"]
	, "text_alpha"			-> "NumberSpec"[1.0]		
	, "text_align"			-> "left"					(* Enum(TextAlign *)
	, "text_baseline"		-> "bottom"					(* Enum(TextBaseline *)
	, "text_line_height"	-> "BFloat"[1.2]
}

ScalarTextProps = {
	"text_font"				-> "BString"["helvetica"]
	, "text_font_size"		-> "FontSize"["12pt"]		(* FontSize(value("12pt")) *)
	, "text_font_style"		-> "normal"					(* Enum(FontStyle *)
	, "text_color"			-> "BColor"["#444444"]
	, "text_alpha"			-> "BPercent"[1.0]		
	, "text_align"			-> "left"					(* Enum(TextAlign *)
	, "text_baseline"		-> "bottom"					(* Enum(TextBaseline *)
	, "text_line_height"	-> "BFloat"[1.2]
}

(* ::Section:: *)
(* Properties  *)

htmlcolornames = {"aliceblue", "antiquewhite", "aqua", "aquamarine", 
   "azure", "beige", "bisque", "black", "blanchedalmond", "blue", 
   "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", 
   "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", 
   "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", 
   "darkgreen", "darkkhaki", "darkmagenta", "darkolivegreen", 
   "darkorange", "darkorchid", "darkred", "darksalmon", 
   "darkseagreen", "darkslateblue", "darkslategray", "darkturquoise", 
   "darkviolet", "deeppink", "deepskyblue", "dimgray", "dodgerblue", 
   "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro", 
   "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", 
   "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", 
   "lavender", "lavenderblush", "lawngreen", "lemonchiffon", 
   "lightblue", "lightcoral", "lightcyan", "lightgoldenrodyellow", 
   "lightgray", "lightgreen", "lightpink", "lightsalmon", 
   "lightseagreen", "lightskyblue", "lightslategray", 
   "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", 
   "magenta", "maroon", "mediumaquamarine", "mediumblue", 
   "mediumorchid", "mediumpurple", "mediumseagreen", 
   "mediumslateblue", "mediumspringgreen", "mediumturquoise", 
   "mediumvioletred", "midnightblue", "mintcream", "mistyrose", 
   "moccasin", "navajowhite", "navy", "oldlace", "olive", "olivedrab",
    "orange", "orangered", "orchid", "palegoldenrod", "palegreen", 
   "paleturquoise", "palevioletred", "papayawhip", "peachpuff", 
   "peru", "pink", "plum", "powderblue", "purple", "red", "rosybrown",
    "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", 
   "seashell", "sienna", "silver", "skyblue", "slateblue", 
   "slategray", "snow", "springgreen", "steelblue", "tan", "teal", 
   "thistle", "tomato", "turquoise", "violet", "wheat", "white", 
   "whitesmoke", "yellow", "yellowgreen"};

BokehColorQ[name_String] /; MemberQ[htmlcolornames, name] := True
BokehColorQ[name_String] /; !StringFreeQ[name, {"rgb(", "rgba("}] := True
BokehColorQ[name_String] /; StringMatchQ[name, StartOfString ~~ "#" ~~ __] := HTMLColorQ[name]
BokehColorQ[_] := False

(* ::Section:: *)
(* Transform  *)

stack[fields_List] := DefaultSpec[expr[StackModel["fields"->fields]]]
stack[] := stack[{}]

(* ::Section:: *)
(* Properties  *)


(* ::Subsection:: *)
(* BBool *)
(*
class Bool(PrimitiveProperty):
    ''' Accept boolean values.

            >>> class BoolModel(HasProps):
            ...     prop = Bool(default=False)
            ...

            >>> m = BoolModel()

            >>> m.prop = True

            >>> m.prop = False

            >>> m.prop = 10  # ValueError !!
*)
Options[BBool] = {"serialized"->True, "readonly"->False}
BBool[v_, opts___] := v


(* ::Subsection:: *)
(* BInt *)
(*
class Int(PrimitiveProperty):
    ''' Accept signed integer values.

            >>> class IntModel(HasProps):
            ...     prop = Int()
            ...

            >>> m = IntModel()

            >>> m.prop = 10

            >>> m.prop = -200

            >>> m.prop = 10.3  # ValueError !!
*)
Options[BInt] = {"serialized"->True, "readonly"->False}
BInt[v_, opts___] := v


(* ::Subsection:: *)
(* BFloat *)
(*
class Float(PrimitiveProperty):
    ''' Accept floating point values.

            >>> class FloatModel(HasProps):
            ...     prop = Float()
            ...

            >>> m = FloatModel()

            >>> m.prop = 10

            >>> m.prop = 10.3

            >>> m.prop = "foo"  # ValueError !!
*)
Options[BFloat] = {"serialized"->True, "readonly"->False}
BFloat[None, opts___] := "None"
BFloat[v_, opts___] := v


(* ::Subsection:: *)
(* BComplex *)
(*
class Complex(PrimitiveProperty):
    ''' Accept complex floating point values.

*)
Options[BComplex] = {"serialized"->True, "readonly"->False}
BComplext[v_, opts___] := v

(* BString *)
(*
class String(PrimitiveProperty):
    ''' Accept string values.

            >>> class StringModel(HasProps):
            ...     prop = String()
            ...

            >>> m = StringModel()

            >>> m.prop = "foo"

            >>> m.prop = 10.3       # ValueError !!

            >>> m.prop = [1, 2, 3]  # ValueError !!
*)
Options[BString] = {"serialized"->True, "readonly"->False}
BString[Null | None, opts___] := Null
BString[v_, opts___] := ToString[v]


(* ::Subsection:: *)
(* Regex *)
(*
class Regex(String):
    ''' Accept strings that match a given regular expression.

            >>> class RegexModel(HasProps):
            ...     prop = Regex("foo[0-9]+bar")
            ...

            >>> m = RegexModel()

            >>> m.prop = "foo123bar"

            >>> m.prop = "foo"      # ValueError !!

            >>> m.prop = [1, 2, 3]  # ValueError !!
*)
Options[Regex] = {"serialized"->True, "readonly"->False}
Regex[v_, opts___] := v


(* ::Subsection:: *)
(* JSON *)
(*
class JSON(String):
    ''' Accept JSON string values.

    The value is transmitted and received by BokehJS as a *string*
    containing JSON content. i.e., you must use ``JSON.parse`` to unpack
    the value into a JavaScript hash.
*)
Options[JSON] = {"serialized"->True, "readonly"->False}
JSON[v_, opts___] := v


(* ::Subsection:: *)
(* Instance *)
(*
class Instance(Property):
    ''' Accept values that are instances of |HasProps|.
*)

Options[Instance] = {"serialized"->True, "readonly"->False}
Instance[Null | None, opts___] := Null
Instance[p : MBokehClassSymbols, opts___] := New[p][]
Instance[p : MBokehClassPatterns, opts___] := 
	Module[{defclass="default"/.{opts}},
		If[IsInstanceQ[p, defclass], (* then *)
			p, (* else *)
			(* some error message *)
			Null
		]
	]

(* ::Subsection:: *)
(* Any *)
(*
class Any(Property):
    ''' Accept all values.

    The ``Any`` property does not do any validation or transformation.
            >>> class AnyModel(HasProps):
            ...     prop = Any()
            ...

            >>> m = AnyModel()

            >>> m.prop = True

            >>> m.prop = 10

            >>> m.prop = 3.14

            >>> m.prop = "foo"

            >>> m.prop = [1, 2, 3]
*)
Options[Any] = {"serialized"->True, "readonly"->False}
Any[v_, opts___] := v


(* ::Subsection:: *)
(* BInterval ?? Should be Range ?*)
(*
class Interval(ParameterizedProperty):
    ''' Accept numeric values that are contained within a given interval.
    
            >>> class RangeModel(HasProps):
            ...     prop = Range(Float, 10, 20)
            ...

            >>> m = RangeModel()

            >>> m.prop = 10

            >>> m.prop = 20

            >>> m.prop = 15

            >>> m.prop = 2     # ValueError !!

            >>> m.prop = 22    # ValueError !!

            >>> m.prop = "foo" # ValueError !!
*)
Options[BInterval] = 
	{"serialized"->True, "readonly"->False,
		"interval_type"->None, "start"->0, "end"->1
	}
BInterval[v_, opts___] := v


(* ::Subsection:: *)
(* BByte *)
(*
class Byte(Interval):
    ''' Accept integral byte values (0-255).

            >>> class ByteModel(HasProps):
            ...     prop = Byte(default=0)
            ...

            >>> m = ByteModel()

            >>> m.prop = 255

            >>> m.prop = 256  # ValueError !!

            >>> m.prop = 10.3 # ValueError !!
*)
Options[BByte] = 
	{"serialized"->True, "readonly"->False}
BByte[v_, opts___] := BInterval[v, "interval_type"->"Int", "start"->0, "end"->255, opts]


(* ::Subsection:: *)
(* Either *)
(*
class Either(ParameterizedProperty):
    ''' Accept values according to a sequence of other property types.

            >>> class EitherModel(HasProps):
            ...     prop = Either(Bool, Int, Auto)
            ...

            >>> m = EitherModel()

            >>> m.prop = True

            >>> m.prop = 10

            >>> m.prop = "auto"

            >>> m.prop = 10.3   # ValueError !!

            >>> m.prop = "foo"  # ValueError !!
*)
Options[Either] = {"serialized"->True, "readonly"->False}
Either[v_, opts___] := v


(* ::Subsection:: *)
(* Enum *)
(*
class Enum(String):
    ''' Accept values from enumerations.

    The first value in enumeration is used as the default value, unless the
    ``default`` keyword argument is used.
*)
Options[Enum] = {"serialized"->True, "readonly"->False}
Enum[v_, opts___] := 
	Module[{},
		BString[v, opts]
	]


(* ::Subsection:: *)
(* Auto *)
(*
class Auto(Enum):
    ''' Accepts only the string "auto".

    Useful for properties that can be configured to behave "automatically".

            >>> class AutoModel(HasProps):
            ...     prop = Either(Float, Auto)
            ...

            >>> m = AutoModel()

            >>> m.prop = 10.2

            >>> m.prop = "auto"

            >>> m.prop = "foo"      # ValueError !!

            >>> m.prop = [1, 2, 3]  # ValueError !!
*)
Options[Auto] = {"serialized"->True, "readonly"->False}
Auto[v_, opts___] := v


(* ::Subsection:: *)
(* RGB *)
(*
class RGB(Property):
    ''' Accept Date (but not DateTime) values.
*)
Options[RGB] = {"serialized"->True, "readonly"->False}
RGB[v_, opts___] := v


(* ::Subsection:: *)
(* BColor *)
(*
class Color(Either):
    ''' Accept color values in a variety of ways.

    For colors, because we support named colors and hex values prefaced
    with a "#", when we are handed a string value, there is a little
    interpretation: if the value is one of the 147 SVG named colors or
    it starts with a "#", then it is interpreted as a value.

    If a 3-tuple is provided, then it is treated as an RGB (0..255).
    If a 4-tuple is provided, then it is treated as an RGBa (0..255), with
    alpha as a float between 0 and 1.  (This follows the HTML5 Canvas API.)

            >>> class ColorModel(HasProps):
            ...     prop = Color()
            ...

            >>> m = ColorModel()

            >>> m.prop = "firebrick"

            >>> m.prop = "#a240a2"

            >>> m.prop = (100, 100, 255)

            >>> m.prop = (100, 100, 255, 0.5)

            >>> m.prop = "junk"              # ValueError !!

            >>> m.prop = (100.2, 57.3, 10.2) # ValueError !!
*)
Options[BColor] = {"serialized"->True, "readonly"->False}
BColor[v_?BokehColorQ, opts___] := v
BColor[v_, ___] := 
	Module[{},
		ValueError["Invalid color (`1`).", v];
		"black"
	]


(* ::Subsection:: *)
(* MinMaxBounds *)
(*
class MinMaxBounds(Either):
    ''' Accept (min, max) bounds tuples for use with Ranges.

    Bounds are provided as a tuple of ``(min, max)`` so regardless of whether your range is
    increasing or decreasing, the first item should be the minimum value of the range and the
    second item should be the maximum. Setting min > max will result in a ``ValueError``.

    Setting bounds to None will allow your plot to pan/zoom as far as you want. If you only
    want to constrain one end of the plot, you can set min or max to
    ``None`` e.g. ``DataRange1d(bounds=(None, 12))`` '''
*)
Options[MinMaxBounds] = {"serialized"->True, "readonly"->False}
MinMaxBounds[v_, opts___] := v


(* ::Subsection:: *)
(* DashPattern *)
(*
class DashPattern(Either):
    ''' Accept line dash specifications.

    Express patterns that describe line dashes.  ``DashPattern`` values
    can be specified in a variety of ways:

    * An enum: "solid", "dashed", "dotted", "dotdash", "dashdot"
    * a tuple or list of integers in the `HTML5 Canvas dash specification style`_.
      Note that if the list of integers has an odd number of elements, then
      it is duplicated, and that duplicated list becomes the new dash list.

    To indicate that dashing is turned off (solid lines), specify the empty
    list [].

    .. _HTML5 Canvas dash specification style: http://www.w3.org/html/wg/drafts/2dcontext/html5_canvas/#dash-list

    '''

    _dash_patterns = {
        "solid": [],
        "dashed": [6],
        "dotted": [2,4],
        "dotdash": [2,4,6,4],
        "dashdot": [6,4,2,4],
    }
*)
Options[DashPattern] = {"serialized"->True, "readonly"->False}
DashPattern[value[v_]	, opts___] := DashPattern[v, opts]
DashPattern["solid"		, opts___] := {}
DashPattern["dashed"	, opts___] := {6}
DashPattern["dotted"	, opts___] := {2,4}
DashPattern["dotdash"	, opts___] := {2,4,6,4}
DashPattern["dashdot"	, opts___] := {6,4,2,4}
DashPattern[l_List, opts___] /; And @@ (IntegerQ /@ l) := 
	If[Length[l]==1 || Mod[Length[l], 2] === 0, l, Join[l,l]]
DashPattern[v_, opts___] := 
	Module[{},	
		ValueError["`1` is not a valid dash spec, using \"solid\" as default.", v];
		{}
	]

(* ::Subsection:: *)
(* BSize *)
(*
class Size(Float):
    ''' Accept non-negative numeric values.

            >>> class SizeModel(HasProps):
            ...     prop = Size()
            ...

            >>> m = SizeModel()

            >>> m.prop = 0

            >>> m.prop = 10e6

            >>> m.prop = -10   # ValueError !!

            >>> m.prop = "foo" # ValueError !!
*)
Options[BSize] = {"serialized"->True, "readonly"->False}
BSize[v_, opts___] := v


(* ::Subsection:: *)
(* BPercent *)
(*
class Percent(Float):
    ''' Accept floating point percentage values.

    ``Percent`` can be useful and semantically meaningful for specifying
    things like alpha values and extents.
*)
Options[BPercent] = {"serialized"->True, "readonly"->False}
BPercent[v_, opts___] := v


(* ::Subsection:: *)
(* Angle *)
(*
class Angle(Float):
    ''' Accept floating point angle values.
*)
Options[Angle] = {"serialized"->True, "readonly"->False}
Angle[v_, opts___] := v


(* ::Subsection:: *)
(* BDate *)
(*
class Date(Property):
    ''' Accept Date (but not DateTime) values.
*)
Options[BDate] = {"serialized"->True, "readonly"->False}
BDate[v_, opts___] := v


(* ::Subsection:: *)
(* Datetime *)
(*
class Datetime(Property):
    ''' Accept Datetime values.
*)
Options[Datetime] = {"serialized"->True, "readonly"->False}
Datetime[v_, opts___] := v


(* ::Subsection:: *)
(* TimeDelta *)
(*
class TimeDelta(Property):
    ''' Accept TimeDelta values.
*)
Options[TimeDelta] = {"serialized"->True, "readonly"->False}
TimeDelta[v_, opts___] := v


(* ::Subsection:: *)
(* Seq *)
(*
class Seq(ContainerProperty):
    ''' Accept non-string ordered sequences of values, e.g. list, tuple, array.
*)
Options[Seq] = {"serialized"->True, "readonly"->False}
Seq[v_, opts___] := v


(* ::Subsection:: *)
(* BList *)
(*
class List(Seq):
    ''' Accept Python list values.
*)
Options[BList] = {"serialized"->True, "readonly"->False}
BList[v_, opts___] := v


(* ::Subsection:: *)
(* BArray *)
(*
class Array(Seq):
    ''' Accept NumPy array values.
*)
Options[BArray] = {"serialized"->True, "readonly"->False}
BArray[v_, opts___] := v


(* ::Subsection:: *)
(* Dict *)
(*
class Dict(ContainerProperty):
    ''' Accept Python dict values.
*)
Options[Dict] = {"serialized"->True, "readonly"->False}
Dict[v_Association, opts___] := v
Dict[v_, opts___] := 
	Module[{},
		ValueError["`1` is not a valid association.", v];
		<||>
	]

(* ::Subsection:: *)
(* ColumnData *)
(*
class ColumnData(Dict):
    ''' Accept a Python dictionary suitable as the ``data`` attribute of a
    :class:`~bokeh.models.sources.ColumnDataSource`.

    This class is a specialization of ``Dict`` that handles efficiently
    encoding columns that are NumPy arrays.
*)
Options[ColumnData] = {"serialized"->True, "readonly"->False}
ColumnData[v_, opts___] := v


(* ::Subsection:: *)
(* Tuple *)
(*
class Tuple(ContainerProperty):
    ''' Accept Python tuple values.
*)
Options[Tuple] = {"serialized"->True, "readonly"->False}
Tuple[v_, opts___] := v


(* ::Subsection:: *)
(* RelativeDelta *)
(*
class RelativeDelta(Dict):
    ''' Accept RelativeDelta dicts for time delta values.
*)
Options[RelativeDelta] = {"serialized"->True, "readonly"->False}
RelativeDelta[v_, opts___] := v


(* ::Subsection:: *)
(* DefaultSpec *)
Options[DefaultSpec] = {"serialized"->True, "readonly"->False}
DefaultSpec[value[None], opts___]	:= <|"value" 	-> Null |>
DefaultSpec[value[v_], opts___]		:= <|"value" 	-> v |>
DefaultSpec[field[f_], opts___]		:= <|"field" 	-> f |>
DefaultSpec[expr[v_], opts___] 		:= <|"expr" 	-> v |>
DefaultSpec[_, ___]					:= <|"value" 	-> Null |>>

(* ::Subsection:: *)
(* DataSpec *)

(*
class DataSpec(Either):
    ''' Base class for properties that accept either a fixed value, or a
    string name that references a column in a
    :class:`~bokeh.models.sources.ColumnDataSource`.

    .. code-block:: python

        glyph.x = 10          # => { 'value': 10 }

        glyph.x = "pressure"  # => { 'field': 'pressure' }
*)
Options[DataSpec] = {"serialized"->True, "readonly"->False}
Condition[ DataSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := a
DataSpec[p:(value | field)[_], opts___] := 
	DefaultSpec[p, opts]
DataSpec[p:(None | Null), opts___] := 
	DefaultSpec[value[p], opts]
Condition[ DataSpec[v_String, opts___],
	StringFreeQ[v,"_"] && NumberQ[ToExpression[v]] ] := DefaultSpec[value[N[ToExpression[v]]], opts]
DataSpec[v_, opts___] := 
	DefaultSpec[field[v], opts]

(* ::Subsection:: *)
(* NumberSpec *)
(*
class NumberSpec(DataSpec):
    ''' A |DataSpec| property that accepts numeric and datetime fixed values.

        m.location = 10.3  # value

        m.location = "foo" # field

*)
Options[NumberSpec] = {"serialized"->True, "readonly"->False, "accept_datetime" -> True}
Condition[ NumberSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] || KeyExistsQ[a, "expr"] ] := a
NumberSpec[p:(value | field | expr)[_], opts___] := 
	DefaultSpec[p, opts]
NumberSpec[p:(Null | None), opts___] := 
	DefaultSpec[value[p], opts]
Condition[ NumberSpec[v_String, opts___],
	StringFreeQ[v,"_"] && NumberQ[ToExpression[v]] ] := DefaultSpec[value[N[ToExpression[v]]], opts]
Condition[ NumberSpec[v_, opts___],
	NumberQ[v] ] := DefaultSpec[value[v], opts]	
Condition[ NumberSpec[v_, opts___],
	AssociationQ[v] || Head[v] === StackModelClass] := DefaultSpec[expr[v], opts]	
NumberSpec[v_, opts___] := 
	DefaultSpec[field[v], opts]


(* ::Subsection:: *)
(* StringSpec *)
(*
class StringSpec(DataSpec):
    ''' A |DataSpec| property that accepts string fixed values.

    Because acceptable fixed values and field names are both strings, it can
    be necessary explicitly to disambiguate these possibilities. By default,
    string values are interpreted as fields, but the |value| function can be
    used to specify that a string should interpreted as a value:

    .. code-block:: python

        m.title = value("foo") # value

        m.title = "foo"        # field

    '''
*)
Options[StringSpec] = {"serialized"->True, "readonly"->False}
Condition[ StringSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := a
StringSpec[p:(value | field)[_], opts___] := 
	DefaultSpec[p, opts]
StringSpec[p:(Null | None), opts___] := 
	DefaultSpec[value[p], opts]
StringSpec[v_String, opts___] := 
	DefaultSpec[field[v], opts]
StringSpec[p_, opts___] := 
	StringSpec[value[ToString[p]], opts]

(* ::Subsection:: *)
(* FontSizeSpec *)
(*
class FontSizeSpec(DataSpec):
    ''' A |DataSpec| property that accepts font-size fixed values.

    The ``FontSizeSpec`` property attempts to first interpret string values as
    font sizes (i.e. valid CSS length values). Otherwise string values are
    interpreted as field names. For example:

    .. code-block:: python

        m.font_size = "10pt"  # value

        m.font_size = "1.5em" # value

        m.font_size = "foo"   # field

    A full list of all valid CSS length units can be found here:

    https://drafts.csswg.org/css-values/#lengths
*)
Options[FontSizeSpec] = {"serialized"->True, "readonly"->False}
Condition[ FontSizeSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := a
FontSizeSpec[p:(value | field)[_], opts___] := 
	DefaultSpec[p, opts]
FontSizeSpec[p:(Null | None), opts___] := 
	DefaultSpec[value[p], opts]
Condition[ FontSizeSpec[f_String, opts___],
	!StringFreeQ[f, {"text_font_size"}] ] := DefaultSpec[field[f], opts]
Condition[ FontSizeSpec[f_String, opts___],
	!StringFreeQ[f, {"pt", "em", "pc", "px", "cm", "mm", "in"}] ] := DefaultSpec[value[f], opts]
Condition[ FontSizeSpec[f_, opts___],
	NumberQ[f] ] := DefaultSpec[value[ToString[Floor[f]]<>"pt"], opts]
FontSizeSpec[v_, opts___] := 
	DefaultSpec[field[v], opts]

(* ::Subsection:: *)
(* UnitsSpec *)

addUnits[a_Association, units_String] := 
	Module[{res=a}, If[!KeyExistsQ[res,"units"],res["units"] = units]; res ]

(*
class UnitsSpec(NumberSpec):
    ''' A |DataSpec| property that accepts numeric fixed values, and also
    provides an associated units property to store units information.
*)
(* TODO: add "units" key *)
Options[UnitsSpec] = {"serialized"->True, "readonly"->False}
Condition[ UnitsSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := a
UnitsSpec[p:(value | field)[_], opts___] := 
	DefaultSpec[p, opts]
UnitsSpec[v_, opts___] :=
	DefaultSpec[value[v], opts]


(* ::Subsection:: *)
(* AngleSpec *)
(*
class AngleSpec(UnitsSpec):
    ''' A |DataSpec| property that accepts numeric fixed values, and also
    provides an associated units property to store angle units.

    Acceptable values for units are ``"rad"`` and ``"deg"``.
*)
(* TODO: Process units ("rad" or "deg") for now default to "rad" *)
$angleDefUnits = "rad"
Options[AngleSpec] = {"serialized"->True, "readonly"->False}
Condition[ AngleSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := addUnits[a, $angleDefUnits]
AngleSpec[p:(value | field)[_], opts___] := 
	addUnits[DefaultSpec[p],$angleDefUnits ]
AngleSpec[p:(Null | None), opts___] := 
	addUnits[DefaultSpec[value[p], opts],$angleDefUnits ]
AngleSpec[v_?NumericQ, opts___] := 
	addUnits[DefaultSpec[value[N[v]], opts], $angleDefUnits ]
AngleSpec[v_String, opts___] := 
	addUnits[DefaultSpec[field[v], opts], $angleDefUnits ]
AngleSpec[v_, opts___] := 
	addUnits[DefaultSpec[value[v], opts], $angleDefUnits ]


(* ::Subsection:: *)
(* DistanceSpec *)
(*
class DistanceSpec(UnitsSpec):
    ''' A |DataSpec| property that accepts numeric fixed values or strings
    that refer to columns in a :class:`~bokeh.models.sources.ColumnDataSource`,
    and also provides an associated units property to store units information.
    Acceptable values for units are ``"screen"`` and ``"data"``.
*)
$distanceDefUnits = "data"
Options[DistanceSpec] = {"serialized"->True, "readonly"->False}
Condition[ DistanceSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := addUnits[a, $distanceDefUnits]
DistanceSpec[p:(value | field)[_], opts___] := 
	addUnits[DefaultSpec[p], $distanceDefUnits]
DistanceSpec[p:(Null | None), opts___] := 
	addUnits[DefaultSpec[value[p], opts], $distanceDefUnits ]
DistanceSpec[v_String, opts___] := 
	addUnits[DefaultSpec[field[v], opts], $distanceDefUnits]
DistanceSpec[v_, opts___] := 
	addUnits[DefaultSpec[value[v], opts], $distanceDefUnits]


(* ::Subsection:: *)
(* ScreenDistanceSpec *)
(*
class ScreenDistanceSpec(NumberSpec):
    ''' A |DataSpec| property that accepts numeric fixed values for screen
    distances, and also provides an associated units property that reports
    ``"screen"`` as the units.

    .. note::
        Units are always ``"screen"``.
*)
$screenDefUnits = "screen"
Options[ScreenDistanceSpec] = {"serialized"->True, "readonly"->False}
Condition[ ScreenDistanceSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := addUnits[a, $screenDefUnits]
ScreenDistanceSpec[p:(value | field)[_], opts___] := 
	addUnits[DefaultSpec[p], $screenDefUnits]
ScreenDistanceSpec[p:(Null | None), opts___] := 
	addUnits[DefaultSpec[value[p], opts], $screenDefUnits ]
ScreenDistanceSpec[v_String, opts___] := 
	addUnits[DefaultSpec[field[v], opts], $screenDefUnits ]
ScreenDistanceSpec[v_, opts___] := 
	addUnits[DefaultSpec[value[v], opts], $screenDefUnits ]


(* ::Subsection:: *)
(* DataDistanceSpec *)
(*
class DataDistanceSpec(NumberSpec):
    ''' A |DataSpec| property that accepts numeric fixed values for data-space
    distances, and also provides an associated units property that reports
    ``"data"`` as the units.

    .. note::
        Units are always ``"data"``.
*)
$dataDefUnits = "data"
Options[DataDistanceSpec] = {"serialized"->True, "readonly"->False}
Condition[ DataDistanceSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := addUnits[a, $dataDefUnits]
DataDistanceSpec[p:(value | field)[_], opts___] := 
	addUnits[DefaultSpec[p], $dataDefUnits]
DataDistanceSpec[p:(Null | None), opts___] := 
	addUnits[DefaultSpec[value[p], opts], $dataDefUnits ]
DataDistanceSpec[v_String, opts___] := 
	addUnits[DefaultSpec[field[v], opts], $dataDefUnits ]
DataDistanceSpec[v_, opts___] := 
	addUnits[DefaultSpec[value[v], opts], $dataDefUnits ]


(* ::Subsection:: *)
(* ColorSpec *)
(*
class ColorSpec(DataSpec):
    ''' A |DataSpec| property that accepts |Color| fixed values.

    The ``ColorSpec`` property attempts to first interpret string values as
    colors. Otherwise, string values are interpreted as field names. For
    example:

    .. code-block:: python

        m.color = "#a4225f"   # value (hex color string)

        m.color = "firebrick" # value (named CSS color string)

        m.color = "foo"       # field (named "foo")

    This automatic interpretation can be override using the dict format
    directly, or by using the |field| function:

    .. code-block:: python

        m.color = { "field": "firebrick" } # field (named "firebrick")

        m.color = field("firebrick")       # field (named "firebrick")
*)

(* TODO: Test for valid colors *)

Options[ColorSpec] = {"serialized"->True, "readonly"->False}
Condition[ ColorSpec[a_Association, opts___],
	KeyExistsQ[a, "field"] || KeyExistsQ[a, "value"] ] := a
Condition[ ColorSpec[a_String, opts___],
	BokehColorQ[a] ] := DefaultSpec[value[a],opts];
ColorSpec[p:(value | field)[_], opts___] := 
	DefaultSpec[p,opts]
ColorSpec[None | Null, opts___] := 
	DefaultSpec[value[Null], opts]
ColorSpec[v_, opts___] :=
	DefaultSpec[field[v], opts]
