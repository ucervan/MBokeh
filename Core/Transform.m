(* Wolfram Language package *)

(*
			, TransformClass
				, CustomJSTransformClass
				, DodgeClass
				, JitterClass
				, InterpolatorClass
					, LinearInterpolatorClass
					, StepInterpolatorClass
				, ColorMapperClass
					, CategoricalColorMapperClass
					, ContinousColorMapperClass
						, LinearColorMapperClass
						, LogColorMapperClass
*)

(* ::Section:: *)
(* FactorCMap *)

FactorCMap[fieldname_String, palette_List, factors_List, start_:0, end_:None, nancolor_:"gray"] :=
	Module[{cm},
		cm = CategoricalColorMapper["palette"->palette, "factors"->factors, "start"->start, "end"->end, "nan_color"->nancolor];
		<|"field" -> fieldname, "transform" -> cm|>
	]

(* ::Section:: *)
(* TransformClass *)

(* abstract *)
(*
    ''' Base class for ``Transform`` models that represent a computation
    to be carried out on the client-side.
*)
TransformClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
		}
	]
	
TransformClass.init[opts : OptionsPattern[TransformClass] ] :=
	Module[{attrs},
		o.type = "Transform";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* CustomJSTransformClass *)

(*
    ''' Apply a custom defined transform to data.

    .. warning::
        The explicit purpose of this Bokeh Model is to embed *raw JavaScript
        code* for a browser to execute. If any part of the code is derived
        from untrusted user inputs, then you must take appropriate care to
        sanitize the user input prior to passing to Bokeh.
*)

CustomJSTransformClass =
	NewClass[
		"Parents"		-> {TransformClass},
		"Fields"		-> {
			"args"		-> "Dict"[<||>]	(* Dict(String, AnyRef, *)
(*			
    A mapping of names to Python objects. In particular those can be bokeh's models.
    These objects are made available to the transform' code snippet as the values of
    named parameters to the callback.
*)
			, "func"	-> BString[""]
(*			
    A snippet of JavaScript code to transform a single value. The variable
    ``x`` will contain the untransformed value and can be expected to be
    present in the function namespace at render time. The snippet will be
    into the body of a function and therefore requires a return statement.

    Example:

        .. code-block:: javascript

            func = '''
            return Math.floor(x) + 0.5
            '''
*)
		, "v_func"		-> "BString"[""]
(*		
    A snippet of JavaScript code to transform an array of values. The variable
    ``xs`` will contain the untransformed array and can be expected to be
    present in the function namespace at render time. The snippet will be
    into the body of a function and therefore requires a return statement.

    Example:

        .. code-block:: javascript

            v_func = '''
            var new_xs = new Array(xs.length)
            for(var i = 0; i < xs.length; i++) {
                new_xs[i] = xs[i] + 0.5
            }
            return new_xs
            '''

    .. warning::
        The vectorized function, ``v_func``, must return an array of the
        same length as the input ``xs`` array.
*)
			, "use_strict"		-> "BBool"[False]
(*
    Enables or disables automatic insertion of ``"use strict";`` into ``func`` or ``v_func``.
*)
		}
	]
	
CustomJSTransformClass.init[opts : OptionsPattern[CustomJSTransformClass] ] :=
	Module[{attrs},
		o.type = "CustomJSTransform";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* DodgeClass *)

(*
    ''' Apply either fixed dodge amount to data.
*)
DodgeClass =
	NewClass[
		"Parents"		-> {TransformClass},
		"Fields"		-> {
			"value"		-> "BFloat"[0]
(*
    The amount to dodge the input data.
*)
			, "range"	-> "Instance"[RangeClass]
(*			
    When applying ``Dodge`` to categorical data values, the corresponding
    ``FactorRange`` must be supplied as the ``range`` property.
*)
		}
	]
	
DodgeClass.init[opts : OptionsPattern[DodgeClass] ] :=
	Module[{attrs},
		o.type = "Dodge";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* JitterClass *)

(*
    ''' Apply either a uniform or normally sampled random jitter to data.
*)
JitterClass =
	NewClass[
		"Parents"		-> {TransformClass},
		"Fields"		-> {
			"mean"		-> "BFloat"[0]
			, "width"		-> "BFloat"[1]
(*
    The width (absolute for uniform distribution and sigma for the normal
    distribution) of the random sample.
*)
			, "distribution"	-> "Enum"["uniform", EnumJitterRandomDistribution]
(*
    The random distribution upon which to pull the random scatter
*)
			, "range"			-> "Instance"[RangeClass]
(*
    When applying Jitter to Categorical data values, the corresponding
    ``FactorRange`` must be supplied as the ``range`` property.
*)
		}
	]
	
JitterClass.init[opts : OptionsPattern[JitterClass] ] :=
	Module[{attrs},
		o.type = "Jitter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* InterpolatorClass *)

(*
    ''' Base class for interpolator transforms.

    Interpolators return the value of a function which has been evaluated
    between specified (x, y) pairs of data.  As an example, if two control
    point pairs were provided to the interpolator, a linear interpolaction
    at a specific value of 'x' would result in the value of 'y' which existed
    on the line conneting the two control points.

    The control point pairs for the interpolators can be specified through either

    * A literal sequence of values:

    .. code-block: python

        interp = Interpolator(x=[1, 2, 3, 4, 5], y=[2, 5, 10, 12, 16])

    * or a pair of columns defined in a `ColumnDataSource` object:

    .. code-block: python

        interp = Interpolator(x="year", y="earnings", data=jewlery_prices))


    This is the base class and is not intended to end use.  Please see the
    documentation for the final derived classes (Jitter, LineraInterpolator,
    StepInterpolator) for mor information on their specific methods of
    interpolation.
*)

InterpolatorClass =
	NewClass[
		"Parents"		-> {TransformClass},
		"Fields"		-> {
			"x"			-> "Either"[""]		(* Either(String, Seq(Float), *)
			, "y"		-> "Either"[""]	 	(* Either(String, Seq(Float), *)
			, "data"	-> "Instance"[ColumnarDataSourceClass]			
			, "clip"	-> "BBool"[True]
		}
	]
	
InterpolatorClass.init[opts : OptionsPattern[InterpolatorClass] ] :=
	Module[{attrs},
		o.type = "Interpolator";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* LinearInterpolatorClass *)

(*
    ''' Compute a linear interpolation between the control points provided through
    the ``x``, ``y``, and ``data`` parameters.
*)

LinearInterpolatorClass =
	NewClass[
		"Parents"		-> {InterpolatorClass},
		"Fields"		-> {
		}
	]
	
LinearInterpolatorClass.init[opts : OptionsPattern[LinearInterpolatorClass] ] :=
	Module[{attrs},
		o.type = "LinearInterpolator";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* StepInterpolatorClass *)

(*
    ''' Compute a step-wise interpolation between the points provided through
    the ``x``, ``y``, and ``data`` parameters.
*)

StepInterpolatorClass =
	NewClass[
		"Parents"		-> {InterpolatorClass},
		"Fields"		-> {
			"mode"		-> "Enum"["after", EnumStepMode]
(*
    Adjust the behavior of the returned value in relation to the control points.  The parameter can assume one of three values:

    * ``after`` (default): Assume the y-value associated with the nearest x-value which is less than or equal to the point to transform.
    * ``before``: Assume the y-value associated with the nearest x-value which is greater than the point to transform.
    * ``center``: Assume the y-value associated with the nearest x-value to the point to transform.
*)	
		}
	]
	
StepInterpolatorClass.init[opts : OptionsPattern[StepInterpolatorClass] ] :=
	Module[{attrs},
		o.type = "StepInterpolator";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* ColorMapperClass *)

ColorMapperClass =
	NewClass[
		"Parents"		-> {TransformClass},
		"Fields"		-> {
			"palette"	-> {}	(* Seq(Color, *)
(*			
    A sequence of colors to use as the target palette for mapping.

    This property can also be set as a ``String``, to the name of any of the
    palettes shown in :ref:`bokeh.palettes`.
    """).accepts(Enum(Palette), lambda pal: getattr(palettes, pal))
*)
			, "nan_color"	-> "BColor"["gray"]
		}
	]
	
ColorMapperClass.init[opts : OptionsPattern[ColorMapperClass] ] :=
	Module[{attrs},
		o.type = "ColorMapper";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* CategoricalColorMapperClass *)

(*
    ''' Map categories to colors. Values that are passed to
    this mapper that aren't in factors will be assigned the nan_color.
*)

CategoricalColorMapperClass =
	NewClass[
		"Parents"		-> {ColorMapperClass},
		"Fields"		-> {
			"factors"		-> "Either"[None] (* (Seq(String), Seq(Tuple(String, String)), Seq(Tuple(String, String, String)), default=None *)
(*			
    A sequence of factors / categories that map to the color palette. For
    example the following color mapper:

    .. code-block:: python

        mapper = CategoricalColorMapper(palette=["red", "blue"], factors=["foo", "bar"])

    will map the factor ``"foo"`` to red and the factor ``"bar"`` to blue.
*)
			, "start"		-> "BInt"[0]
(*			
    A start index to "slice" data factors with before color mapping.

    For example, if the data to color map consists of 2-level factors such
    as ``["2016", "sales"]`` and ``["2016", "marketing"]``, then setting
    ``start=1`` will perform color mapping only based on the second sub-factor
    (i.e. in this case based on the department ``"sales"`` or ``"marketing"``)
*)
			, "end"			-> "BInt"[Null]
(*
    A start index to "slice" data factors with before color mapping.

    For example, if the data to color map consists of 2-level factors such
    as ``["2016", "sales"]`` and ``["2017", "marketing"]``, then setting
    ``end=1`` will perform color mapping only based on the first sub-factor
    (i.e. in this case based on the year ``"2016"`` or ``"2017"``)

    If ``None`` then all sub-factors from ``start`` to the end of the
    factor will be used for color mapping.
*)
		}
	]
	
CategoricalColorMapperClass.init[opts : OptionsPattern[CategoricalColorMapperClass] ] :=
	Module[{attrs},
		o.type = "CategoricalColorMapper";
(*
    def __init__(self, **kwargs):
        super(ColorMapper, self).__init__( **kwargs)
        palette = self.palette
        factors = self.factors
        if palette is not None and factors is not None:
            if len(palette) < len(factors):
                extra_factors = factors[len(palette):]
                warnings.warn("Palette length does not match number of factors. %s will be assigned to `nan_color` %s" % (extra_factors, self.nan_color))
*)

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* ContinousColorMapperClass *)

ContinousColorMapperClass =
	NewClass[
		"Parents"		-> {ColorMapperClass},
		"Fields"		-> {
			"low"		-> "BFloat"[Null]
(*			
    The minimum value of the range to map into the palette. Values below
    this are clamped to ``low``.
*)
			, "high"		-> "BFloat"[Null]
(*			
    The maximum value of the range to map into the palette. Values above
    this are clamped to ``high``.
*)
			, "low_color"	-> "BColor"[None]
(*			
    Color to be used if data is lower than ``low`` value. If None,
    values lower than ``low`` are mapped to the first color in the palette.
*)
			, "high_color"	->	"BColor"[None]
(*
    Color to be used if data is higher than ``high`` value. If None,
    values higher than ``high`` are mapped to the last color in the palette.
*)
		}
	]
	
ContinousColorMapperClass.init[opts : OptionsPattern[ContinousColorMapperClass] ] :=
	Module[{attrs},
		o.type = "ContinousColorMapper";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* LinearColorMapperClass *)

(*
    ''' Map numbers in a range [*low*, *high*] linearly into a sequence of
    colors (a palette).

    For example, if the range is [0, 99] and the palette is
    ``['red', 'green', 'blue']``, the values would be mapped as follows::

             x < 0  : 'red'     # values < low are clamped
        0 >= x < 33 : 'red'
       33 >= x < 66 : 'green'
       66 >= x < 99 : 'blue'
       99 >= x      : 'blue'    # values > high are clamped
*)

LinearColorMapperClass =
	NewClass[
		"Parents"		-> {ContinousColorMapperClass},
		"Fields"		-> {
		}
	]
	
LinearColorMapperClass.init[opts : OptionsPattern[LinearColorMapperClass] ] :=
	Module[{attrs},
		o.type = "LinearColorMapper";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* LogColorMapperClass *)

(*
    ''' Map numbers in a range [*low*, *high*] into a sequence of colors
    (a palette) on a natural logarithm scale.

    For example, if the range is [0, 25] and the palette is
    ``['red', 'green', 'blue']``, the values would be mapped as follows::

                x < 0     : 'red'     # values < low are clamped
       0     >= x < 2.72  : 'red'     # math.e ** 1
       2.72  >= x < 7.39  : 'green'   # math.e ** 2
       7.39  >= x < 20.09 : 'blue'    # math.e ** 3
       20.09 >= x         : 'blue'    # values > high are clamped

    .. warning::
        The LogColorMapper only works for images with scalar values that are
        non-negative.
*)

LogColorMapperClass =
	NewClass[
		"Parents"		-> {ContinousColorMapperClass},
		"Fields"		-> {
		}
	]
	
LogColorMapperClass.init[opts : OptionsPattern[LogColorMapperClass] ] :=
	Module[{attrs},
		o.type = "LogColorMapper";

		(* attribute fields *)
		attrs = o.attributes;
	]
