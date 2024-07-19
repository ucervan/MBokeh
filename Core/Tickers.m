(* Wolfram Language package *)

(*
	ModelClass
		, TickerClass
			, ContinuousTickerClass
				, FixedTickerClass
				, AdaptiveTickerClass
					, BasicTickerClass
						, MercatorTickerClass
					, LogTickerClass
				, CompositeTickerClass
					, DatetimeTickerClass
				, SingleIntervalTickerClass
					, DaysTickerClass
					, MonthsTickerClass
					, YearsTickerClass
			, CategoricalTickerClass
*)

(* ::Section:: *)
(* TickerClass *)

TickerClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
		}
	]
	
TickerClass.init[opts : OptionsPattern[TickerClass] ] :=
	Module[{attrs},
		o.type = "Ticker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ContinuousTickerClass *)

ContinuousTickerClass =
	NewClass[
		"Parents"		-> {TickerClass},
		"Fields"		-> {
			"num_minor_ticks"	-> "BInt"[5]
			, "desired_num_ticks"	-> "BInt"[6]
		}
	]
	
ContinuousTickerClass.init[opts : OptionsPattern[ContinuousTickerClass] ] :=
	Module[{attrs},
		o.type = "ContinuousTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* FixedTickerClass *)

FixedTickerClass =
	NewClass[
		"Parents"		-> {ContinuousTickerClass},
		"Fields"		-> {
    		"ticks" 		-> {} (* Seq(Float, default=[], *)
		}
	]
	
FixedTickerClass.init[opts : OptionsPattern[FixedTickerClass] ] :=
	Module[{attrs},
		o.type = "FixedTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* AdaptiveTickerClass *)
(*
class AdaptiveTicker(ContinuousTicker):
    ''' Generate "nice" round ticks at any magnitude.

    Creates ticks that are "base" multiples of a set of given
    mantissas. For example, with ``base=10`` and
    ``mantissas=[1, 2, 5]``, the ticker will generate the sequence::

        ..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, ...

    '''
*)

AdaptiveTickerClass =
	NewClass[
		"Parents"		-> {ContinuousTickerClass},
		"Fields"		-> {
			"base"			-> "BFloat"[10.0]
			, "mantissas"	-> {}				(* Seq(Float, [1, 2, 5], *)
			, "min_interval"	-> "BFloat"[0.0]
			, "max_interval"	-> "BFloat"[None]
		}
	]
	
AdaptiveTickerClass.init[opts : OptionsPattern[AdaptiveTickerClass] ] :=
	Module[{attrs},
		o.type = "AdaptiveTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* BasicTickerClass *)

BasicTickerClass =
	NewClass[
		"Parents"		-> {AdaptiveTickerClass},
		"Fields"		-> {
		}
	]
	
BasicTickerClass.init[opts : OptionsPattern[BasicTickerClass] ] :=
	Module[{attrs},
		o.type = "BasicTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MercatorTickerClass *)

MercatorTickerClass =
	NewClass[
		"Parents"		-> {BasicTickerClass},
		"Fields"		-> {
    		"dimension"		-> "None" (* Enum(LatLon, default=None, *)
		}
	]
	
MercatorTickerClass.init[opts : OptionsPattern[MercatorTickerClass] ] :=
	Module[{attrs},
		o.type = "MercatorTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* LogTickerClass *)

LogTickerClass =
	NewClass[
		"Parents"		-> {AdaptiveTickerClass},
		"Fields"		-> {
    		"mantissas" 	-> {1,5}	(* Override(default=[1, 5]) *)
		}
	]
	
LogTickerClass.init[opts : OptionsPattern[LogTickerClass] ] :=
	Module[{attrs},
		o.type = "LogTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CompositeTickerClass *)

CompositeTickerClass =
	NewClass[
		"Parents"		-> {ContinuousTickerClass},
		"Fields"		-> {
			"tickers"		-> List[TickerClass] (* Seq(Instance(Ticker), default=[] *)
		}
	]
	
CompositeTickerClass.init[opts : OptionsPattern[CompositeTickerClass] ] :=
	Module[{attrs},
		o.type = "CompositeTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Subsection:: *)
(* DatetimeTickerClass *)

ONEMILLI = 1.0
ONESECOND = 1000.0
ONEMINUTE = 60.0 * ONESECOND
ONEHOUR = 60 * ONEMINUTE
ONEDAY = 24 * ONEHOUR
ONEMONTH = 30 * ONEDAY (* An approximation, obviously.*)
ONEYEAR = 365 * ONEDAY

DatetimeTickerClass =
	NewClass[
		"Parents"		-> {CompositeTickerClass},
		"Fields"		-> {
			"tickers"		-> List[TickerClass] (* Seq(Instance(Ticker), default=[] *)
		}
	]
	
DatetimeTickerClass.init[opts : OptionsPattern[DatetimeTickerClass] ] :=
	Module[{attrs, tickers},
		o.type = "DatetimeTicker";

		tickers = {
			New[AdaptiveTickerClass][
				"mantissas" 		-> {1,2,5},	
				"base"				-> 10,
				"min_interval"		-> 0,
				"max_interval"		-> 500*ONEMILLI,
				"num_minor_ticks"	-> 0
			],
			New[AdaptiveTickerClass][
				"mantissas" 		-> {1, 2, 5, 10, 15, 20, 30},	
				"base"				-> 60,
				"min_interval"		-> ONESECOND,
				"max_interval"		-> 30*ONEMINUTE,
				"num_minor_ticks"	-> 0
			],
			New[AdaptiveTickerClass][
				"mantissas" 		-> {1, 2, 4, 6, 8, 12},
				"base"				-> 24,
				"min_interval"		-> ONEHOUR,
				"max_interval"		-> 12*ONEHOUR,
				"num_minor_ticks"	-> 0
			],
			New[DaysTickerClass][
				"days"	-> Range[1,32]				
			],
			New[DaysTickerClass][
				"days"	-> Range[1,31,3]
			],
			New[DaysTickerClass][
				"days"	-> {1, 8, 15, 22}
			],
			New[DaysTickerClass][
				"days"	-> {1, 15}
			],
			New[MonthsTickerClass][
				"months"-> Range[0,12,1] 
			],
			New[MonthsTickerClass][
				"months"-> Range[0,12,2] 
			],
			New[MonthsTickerClass][
				"months"-> Range[0,12,4] 
			],
			New[MonthsTickerClass][
				"months"-> Range[0,12,6] 
			],
			New[YearsTickerClass][]			
		};

		o."tickers".stuff[tickers];
		
		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* SingleIntervalTickerClass *)

SingleIntervalTickerClass =
	NewClass[
		"Parents"		-> {ContinuousTickerClass},
		"Fields"		-> {
    		"interval"		-> "BFloat"[0]
		}
	]
	
SingleIntervalTickerClass.init[opts : OptionsPattern[SingleIntervalTickerClass] ] :=
	Module[{attrs},
		o.type = "SingleIntervalTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DaysTickerClass *)

DaysTickerClass =
	NewClass[
		"Parents"		-> {SingleIntervalTickerClass},
		"Fields"		-> {
			"days"				-> {} 			(* Seq(Int, default=[], *) 
			, "num_minor_ticks"	-> "BInt"[0] 	(* Override(default=0) *)
		}
	]
	
DaysTickerClass.init[opts : OptionsPattern[DaysTickerClass] ] :=
	Module[{attrs},
		o.type = "DaysTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MonthsTickerClass *)

MonthsTickerClass =
	NewClass[
		"Parents"		-> {SingleIntervalTickerClass},
		"Fields"		-> {
			"months"	-> {} 	(* Seq(Int, default=[] *)
		}
	]
	
MonthsTickerClass.init[opts : OptionsPattern[MonthsTickerClass] ] :=
	Module[{attrs},
		o.type = "MonthsTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* YearsTickerClass *)

YearsTickerClass =
	NewClass[
		"Parents"		-> {SingleIntervalTickerClass},
		"Fields"		-> {
		}
	]
	
YearsTickerClass.init[opts : OptionsPattern[YearsTickerClass] ] :=
	Module[{attrs},
		o.type = "YearsTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CategoricalTickerClass *)

CategoricalTickerClass =
	NewClass[
		"Parents"		-> {TickerClass},
		"Fields"		-> {
		}
	]
	
CategoricalTickerClass.init[opts : OptionsPattern[CategoricalTickerClass] ] :=
	Module[{attrs},
		o.type = "CategoricalTicker";

		(* attribute fields *)
		attrs = o.attributes;
	]

