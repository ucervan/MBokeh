(* Wolfram Language package *)

(*
	ModelClass
		, TickFormatterClass
			, BasicTickFormatterClass
				, MercatorTickFormatterClass
			, NumeralTickFormatterClass
			, PrintfTickFormatterClass
			, LogTickFormatterClass
			, CategoricalTickFormatterClass
			, FuncTickFormatterClass
			, DatetimeTickFormatterClass
*)


(* ::Section:: *)
(* TickFormatterClass *)

TickFormatterClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
		}
	]
	
TickFormatterClass.init[opts : OptionsPattern[TickFormatterClass] ] :=
	Module[{attrs},
		o.type = "TickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* BasicTickFormatterClass *)

BasicTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
			"precision"				-> "auto" 		(* Either(Auto, Int *)
			, "use_scientific"		-> "BBool"[True] 
			, "power_limit_high"	-> "BInt"[5]
			, "power_limit_low"		-> "BInt"[-3]
		}
	]
	
BasicTickFormatterClass.init[opts : OptionsPattern[BasicTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "BasicTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MercatorTickFormatterClass *)

MercatorTickFormatterClass =
	NewClass[
		"Parents"		-> {BasicTickFormatterClass},
		"Fields"		-> {
			"dimension"		-> "None"	(* Enum(LatLon, default=None *)
(*			
    Specify whether to format ticks for Latitude or Longitude.

    Projected coordinates are not separable, computing Latitude and Longitude
    tick labels from Web Mercator requires considering coordinates from both
    dimensions together. Use this property to specify which result should be
    used for display.

    Typically, if the formatter is for an x-axis, then dimension should be
    ``"lon"`` and if the formatter is for a y-axis, then the dimension
    should be `"lat"``.

    In order to prevent hard to debug errors, there is no default value for
    dimension. Using an un-configured MercatorTickFormatter will result in
    a validation error and a JavaScript console error.
*)			
		}
	]
	
MercatorTickFormatterClass.init[opts : OptionsPattern[MercatorTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "MercatorTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* NumeralTickFormatterClass *)

NumeralTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
			"format"		-> "BString"["0,0"]  	(* String("0,0", *)
(*
    The number format, as defined in the following tables:

    **NUMBERS**:

    ============ ============== ===============
    Number       Format         String
    ============ ============== ===============
    10000        '0,0.0000'     10,000.0000
    10000.23     '0,0'          10,000
    10000.23     '+0,0'         +10,000
    -10000       '0,0.0'        -10,000.0
    10000.1234   '0.000'        10000.123
    10000.1234   '0[.]00000'    10000.12340
    -10000       '(0,0.0000)'   (10,000.0000)
    -0.23        '.00'          -.23
    -0.23        '(.00)'        (.23)
    0.23         '0.00000'      0.23000
    0.23         '0.0[0000]'    0.23
    1230974      '0.0a'         1.2m
    1460         '0 a'          1 k
    -104000      '0a'           -104k
    1            '0o'           1st
    52           '0o'           52nd
    23           '0o'           23rd
    100          '0o'           100th
    ============ ============== ===============

    **CURRENCY**:

    =========== =============== =============
    Number      Format          String
    =========== =============== =============
    1000.234    '$0,0.00'       $1,000.23
    1000.2      '0,0[.]00 $'    1,000.20 $
    1001        '$ 0,0[.]00'    $ 1,001
    -1000.234   '($0,0)'        ($1,000)
    -1000.234   '$0.00'         -$1000.23
    1230974     '($ 0.00 a)'    $ 1.23 m
    =========== =============== =============

    **BYTES**:

    =============== =========== ============
    Number          Format      String
    =============== =========== ============
    100             '0b'        100B
    2048            '0 b'       2 KB
    7884486213      '0.0b'      7.3GB
    3467479682787   '0.000 b'   3.154 TB
    =============== =========== ============

    **PERCENTAGES**:

    ============= ============= ===========
    Number        Format        String
    ============= ============= ===========
    1             '0%'          100%
    0.974878234   '0.000%'      97.488%
    -0.43         '0 %'         -43 %
    0.43          '(0.000 %)'   43.000 %
    ============= ============= ===========

    **TIME**:

    ============ ============== ============
    Number       Format         String
    ============ ============== ============
    25           '00:00:00'     0:00:25
    238          '00:00:00'     0:03:58
    63846        '00:00:00'     17:44:06
    ============ ============== ============

    For the complete specification, see http://numbrojs.com/format.html
*)
	, "language"		-> "en"		(* Enum(NumeralLanguage, default="en" *)
	, "rounding"		-> "round"	(* Enum(RoundingFunction *)
		}
	]
	
NumeralTickFormatterClass.init[opts : OptionsPattern[NumeralTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "NumeralTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* PrintfTickFormatterClass *)

PrintfTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
			"format"		-> "BString"["%s"]		(* String("%s", *)
(*
    The number format, as defined as follows: the placeholder in the format
    string is marked by % and is followed by one or more of these elements,
    in this order:

    * An optional ``+`` sign
        Causes the result to be preceded with a plus or minus sign on numeric
        values. By default, only the ``-`` sign is used on negative numbers.

    * An optional padding specifier
        Specifies what (if any) character to use for padding. Possible values
        are 0 or any other character preceded by a ``'`` (single quote). The
        default is to pad with spaces.

    * An optional ``-`` sign
        Causes sprintf to left-align the result of this placeholder. The default
        is to right-align the result.

    * An optional number
        Specifies how many characters the result should have. If the value to be
        returned is shorter than this number, the result will be padded.

    * An optional precision modifier
        Consists of a ``.`` (dot) followed by a number, specifies how many digits
        should be displayed for floating point numbers. When used on a string, it
        causes the result to be truncated.

    * A type specifier
        Can be any of:

        - ``%`` --- yields a literal ``%`` character
        - ``b`` --- yields an integer as a binary number
        - ``c`` --- yields an integer as the character with that ASCII value
        - ``d`` or ``i`` --- yields an integer as a signed decimal number
        - ``e`` --- yields a float using scientific notation
        - ``u`` --- yields an integer as an unsigned decimal number
        - ``f`` --- yields a float as is
        - ``o`` --- yields an integer as an octal number
        - ``s`` --- yields a string as is
        - ``x`` --- yields an integer as a hexadecimal number (lower-case)
        - ``X`` --- yields an integer as a hexadecimal number (upper-case)
*)
		}
	]
	
PrintfTickFormatterClass.init[opts : OptionsPattern[PrintfTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "PrintfTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* LogTickFormatterClass *)

LogTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
			"ticker"		-> "Instance"[TickerClass] 		(* Instance(Ticker, *)
(*
    The corresponding ``LogTicker``, used to determine the correct
    base to use. If unset, the formatter will use base 10 as a default.
*)
		}
	]
	
LogTickFormatterClass.init[opts : OptionsPattern[LogTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "LogTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CategoricalTickFormatterClass *)

CategoricalTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
		}
	]
	
CategoricalTickFormatterClass.init[opts : OptionsPattern[CategoricalTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "CategoricalTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* FuncTickFormatterClass *)

FuncTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
			"args"		-> <||>		(* Dict(String, Instance(Model), *)
(*
    A mapping of names to Bokeh plot objects. These objects are made
    available to the formatter code snippet as the values of named
    parameters to the callback.
*)
			, "code"	-> "BString"[""] 	(* String(default="", *)
(*
    A snippet of JavaScript code that reformats a single tick to the desired
    format. The variable ``tick`` will contain the unformatted tick value and
    can be expected to be present in the code snippet namespace at render time.

    Example:

        .. code-block:: javascript

            code = '''
            return Math.floor(tick) + " + " + (tick % 1).toFixed(2)
            '''
*)
		}
	]
	
FuncTickFormatterClass.init[opts : OptionsPattern[FuncTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "FuncTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DatetimeTickFormatterClass *)

DatetimeTickFormatterClass =
	NewClass[
		"Parents"		-> {TickFormatterClass},
		"Fields"		-> {
				"microseconds"		-> {"%fus"}
				, "milliseconds"	-> {"%3Nms", "%S.%3Ns"}
				, "seconds"			-> {"%Ss"}
				, "minsec"			-> {":%M:%S"}
				, "minutes"			-> {":%M", "%Mm"}
				, "hourmin"			-> {"%H:%M"}
				, "hours"			-> {"%Hh", "%H:%M"}
				, "days"			-> {"%m/%d", "%a%d"}
				, "months"			-> {"%m/%Y", "%b%y"}
				, "years"			-> {"%Y"}
		}
	]
	
DatetimeTickFormatterClass.init[opts : OptionsPattern[DatetimeTickFormatterClass] ] :=
	Module[{attrs},
		o.type = "DatetimeTickFormatter";

		(* attribute fields *)
		attrs = o.attributes;
	]

