(* Wolfram Language package *)

enumeration = List

InEnumerationQ[v_, enum_String] := InEnumerationQ[v, ToExpression[ "MBokeh`Private`Enum"<>enum]]
InEnumerationQ[v_, enum_] := MemberQ[enum, v]

(*

''' Common enumerations to be used together with |Enum| property.

This module provides many pre-defined enumerations, as well as functions
for creating new enumerations.

New enumerations can be created using the |enumeration| function:

.. code-block:: python

    #: Specify a nautically named side, port or starboard
    MyEnum = enumeration("port", "starboard")

Typically, enumerations are used to define |Enum| properties:

.. code-block:: python

    from bokeh.model import Model
    from bokeh.core.properties import Enum

    class MyModel(Model):

        location = Enum(MyEnum, help="""
        Whether the thing should be a port or starboard.
        """)

Enumerations have a defined order and support iteration:

.. code-block:: python

    >>> for loc in MyEnum:
    ...     print(loc)
    ...
    port
    starboard

as well as containment tests:

.. code-block:: python

    >>> "port" in MyEnum
    True

Enumerations can be easily documented in Sphinx documentation with the
:ref:`bokeh.sphinxext.bokeh_enum` Sphinx extension.

----

.. autofunction:: bokeh.core.enums.enumeration

----

.. |Enum| replace:: :class:`~bokeh.core.properties.Enum`
.. |enumeration| replace:: :func:`~bokeh.core.enums.enumeration`

'''

from __future__ import absolute_import

from six import string_types

from .. import colors, palettes

class Enumeration(object):
    ''' Represent an enumerated collection of values.

    .. note::
        Instances of ``Enumeration`` typically should not be constructed
        directly. Instead, use the |enumeration| function.

    '''
    __slots__ = ()

    def __iter__(self):
        return iter(self._values)

    def __contains__(self, value):
        if not self._case_sensitive:
            value = value.lower()
        return value in self._values

    def __str__(self):
        return "Enumeration(%s)" % ", ".join(self._values)

    __repr__ = __str__

def enumeration( *values, **kwargs):
    ''' Create an |Enumeration| object from a sequence of values.

    Call ``enumeration`` with a sequence of (unique) strings to create an
    Enumeration object:

    .. code-block:: python

        #: Specify the horizontal alignment for rendering text
        TextAlign = enumeration("left", "right", "center")

    Args:
        values (str) : string enumeration values, passed as positional arguments

            The order of arguments is the order of the enumeration, and the
            first element will be considered the default value when used
            to create |Enum| properties.

    Keyword Args:
        case_sensitive (bool, optional) :
            Whether validation should consider case or not (default: True)

    Raises:
        ValueError if values empty, if any value is not a string or not unique

    Returns:
        Enumeration

    '''
    if not (values and all(isinstance(value, string_types) and value for value in values)):
        raise ValueError("expected a non-empty sequence of strings, got %s" % values)

    if len(values) != len(set(values)):
        raise ValueError("enumeration items must be unique, got %s" % values)

    attrs = dict([ (value, value) for value in values ])
    attrs.update({
        "_values": list(values),
        "_default": values[0],
        "_case_sensitive": kwargs.get("case_sensitive", True),
    })

    return type("Enumeration", (Enumeration,), attrs)()

#: Specify whether events should be combined or collected as-is when a Document hold is in effect
HoldPolicy = enumeration( "combine", "collect")


*)

(* Specify whether a dimension or coordinate is latitude or longitude *)
EnumLatLon = enumeration["lat", "lon"]

(* Specify how stroked lines should be joined together *)
EnumLineJoin = enumeration["miter", "round", "bevel"]

(* Specify a named dash pattern for stroking lines *)
EnumLineDash = enumeration["solid", "dashed", "dotted", "dotdash", "dashdot"]

(* Specify how stroked lines should be terminated *)
EnumLineCap = enumeration["butt", "round", "square"]

(* Specify the font style for rendering text *)
EnumFontStyle = enumeration["normal", "italic", "bold"]

(* Specify the vertical alignment for rendering text *)
EnumVerticalAlign = enumeration["top", "middle", "bottom"]

(* Specify the horizontal alignment for rendering text *)
EnumTextAlign = enumeration["left", "right", "center"]

(* Specify the baseline location for rendering text *)
EnumTextBaseline = enumeration["top", "middle", "bottom", "alphabetic", "hanging", "ideographic"]

(* Specify a stroke direction for circles, wedges, etc. *)
EnumDirection = enumeration["clock", "anticlock"]

(* Specify units for mapping values *)
EnumSpatialUnits = enumeration["screen", "data"]

(* Specify the units for an angle value *)
EnumAngleUnits = enumeration["deg", "rad"]

(* Specify a date/time scale *)
EnumDatetimeUnits = enumeration["microseconds", "milliseconds", "seconds", "minsec",
                            "minutes", "hourmin", "hours", "days", "months", "years"]

(*	Specify a vertical/horizontal dimension *)
EnumDimension = enumeration["width", "height"]

(*	Specify a vertical/horizontal dimensions *)
EnumDimensions = enumeration["width", "height", "both"]

(*	Specify a vertical/horizontal orientation for something *)
EnumOrientation = enumeration["horizontal", "vertical"]

(*	Specify a fixed location for a Bokeh legend *)
EnumLegendLocation = EnumAnchor = enumeration[
    "top_left",    "top_center",    "top_right",
    "center_left", "center",        "center_right",
    "bottom_left", "bottom_center", "bottom_right"]

(*	Specify a location in plot layouts *)
EnumLocation = enumeration["above", "below", "left", "right"]

(*	Specify a vertical location in plot layouts *)
EnumVerticalLocation = enumeration["above", "below"]

(*	Specify a horizontal location in plot layouts *)
EnumHorizontalLocation = enumeration["left", "right"]

(* Specify an attachment for tooltips *)
EnumTooltipAttachment = enumeration["horizontal", "vertical",
                                "left", "right", "above", "below"]

(* Specify a named dashing patter for stroking lines *)
EnumDashPattern = enumeration["solid", "dashed", "dotted", "dotdash", "dashdot"]

(*	Specify a style for button widgets *)
EnumButtonType = enumeration["default", "primary", "success", "warning", "danger", "link"]

(*
#: Specify one of the 137 named CSS colors
NamedColor = enumeration( *colors.named.__all__, case_sensitive=False)

#: Specify the name of a palette from :ref:`bokeh.palettes`
Palette = enumeration( *palettes.__palettes__)

*)

(* 	Specify a style for a Google map *)
EnumMapType = enumeration["satellite", "roadmap", "terrain", "hybrid"]

(* Specify a format for printing dates *)
EnumDateFormat = enumeration["ATOM", "W3C", "RFC-3339", "ISO-8601", "COOKIE", "RFC-822",
                         "RFC-850", "RFC-1036", "RFC-1123", "RFC-2822", "RSS", "TIMESTAMP"]

(*Specify a policy for  how numbers should be rounded *)
EnumRoundingFunction = enumeration["round", "nearest", "floor", "rounddown", "ceil", "roundup"]

(* Specify a locale for printing numeric values *)
EnumNumeralLanguage = enumeration["be-nl", "chs", "cs", "da-dk", "de-ch", "de", "en",
                              "en-gb", "es-ES", "es", "et", "fi", "fr-CA", "fr-ch",
                              "fr", "hu", "it", "ja", "nl-nl", "pl", "pt-br",
                              "pt-pt", "ru", "ru-UA", "sk", "th", "tr", "uk-UA"]

(* Specify an output backend to render a plot area onto *)
EnumOutputBackend = enumeration["canvas", "svg", "webgl"]

(* Specify a position in the render order for a Bokeh renderer *)
EnumRenderLevel = enumeration["image", "underlay", "glyph", "annotation", "overlay"]

(* Specify a render mode for renderers that support both Canvas or CSS rendering *)
EnumRenderMode = enumeration["canvas", "css"]

(* Specify a start/end value *)
EnumStartEnd = enumeration["start", "end"]

(*Specify a mode for stepwise interpolation *)
EnumStepMode = enumeration["before", "after", "center"]

(* Specify different callback policies for the slider widget *)
EnumSliderCallbackPolicy = enumeration["continuous", "throttle", "mouseup"]

(* Specify a distribution to use for the Jitter class *)
EnumJitterRandomDistribution = enumeration["uniform", "normal"]

(* Specify sorting directions *)
SortDirection = enumeration["ascending", "descending"]

(* Sizing mode policies *)
EnumSizingMode = enumeration["stretch_both", "scale_width", "scale_height", "scale_both", "fixed"]

(* Specify how a legend should respond to click events *)
EnumLegendClickPolicy = enumeration["none", "hide", "mute"]

(* Whether range padding should be interpreted a percentage or and absolute quantity *)
EnumPaddingUnits = enumeration["percent", "absolute"]

(* Specify how axis tick labels are oriented with respect to the axis *)
EnumTickLabelOrientation = enumeration["horizontal", "vertical", "parallel", "normal"]

(* Specify how a format string for a tooltip field should be interpreted *)
EnumTooltipFieldFormatter = enumeration["numeral", "datetime", "printf"]

