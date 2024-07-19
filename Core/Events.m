(* Wolfram Language package *)

CONCRETEEVENTCLASSES = <||>

MetaEvent[class_] :=
	Module[{ename},
		ename = "event_name" /. Options[class];
		If[!MatchQ[ename, ("event_name" | None)],
			CONCRETEEVENTCLASSES[ename] = class
		]
	]

(* ::Section:: *)
(* Events *)

EventClass = 
	NewClass[
		"Parents" 	-> {BaseClass},
		"Fields"	-> {
			"_event_classes"	-> {}
			, "model"			-> None
			, "event_name"		-> None
			, "_model_id"		-> None
		}
	]
EventClass.init[opts : OptionsPattern[] ] :=
	Module[{},
			o."_model_id" = None;
			If[o."model" =!= None,
				o."_model_id" = o.model."id"
			];			
	]
EventClass.tableform[opts___] := TableForm[Transpose[{o.getFields[], o.getFieldValues[]}], opts]
	
EventClass.decodejson[dct_String] :=
	Module[{dic},
		dic = ImportString[StringReplace[dct, "'"-> "\""], "RawJSON"];
		If[!(KeyExistsQ[dic, "event_name"] && KeyExistsQ[dic, "event_values"]),
			Return[dct]
		];
		o.decodejson[dic]
	]
EventClass.decodejson[dct_Association] :=
	Module[{modelid, event, eventvalues = dct["event_values"], eventname = dct["event_name"]},
		If[MissingQ[eventname] || MissingQ[CONCRETEEVENTCLASSES[eventname]],
			ValueError["Could not find appropiate Event class for event_name: `1`", eventname];
			Return[$Failed];
		];
		modelid = eventvalues["model_id"];
		event = New[CONCRETEEVENTCLASSES[eventname]]["model"->None, Sequence@@Normal@eventvalues];
		event."_model_id" = modelid;
		event
	]
	
(* ::Subsection:: *)
(* ButtonClick *)
	
ButtonClickClass = 
	NewClass[
		"Parents" 	-> {EventClass},
		"Fields"	-> {
			"event_name" 	-> "button_click"
		}
	]
ButtonClickClass.init[opts : OptionsPattern[ButtonClickClass]] :=
	Module[{},
		If[o.model=!=None && !IsInstanceQ[o.model, ButtonWidgetClass],
			ValueError["`1` event only applies to ButtonWidget models", ButtonClick]
		];
	]
MetaEvent[ButtonClickClass]
	

(* ::Subsection:: *)
(* PlotEventClass *)
	
PlotEventClass = 
	NewClass[
		"Parents" 	-> {EventClass},
		"Fields"	-> {
		}
	]
PlotEventClass.init[opts : OptionsPattern[PlotEventClass]] :=
	Module[{},
		If[o.model=!=None && !IsInstanceQ[o.model, PlotModelClass],
			ValueError["`1` event only applies to PlotModelClass models", Head[o]]
		];
	]
	
	
(* ::Subsection:: *)
(* LODStartClass *)
	
LODStartClass = 
	NewClass[
		"Parents" 	-> {PlotEventClass},
		"Fields"	-> {
			"event_name" 	-> "lodstart"
		}
	]
MetaEvent[LODStartClass]


(* ::Subsection:: *)
(* LODEndClass *)
	
LODEndClass = 
	NewClass[
		"Parents" 	-> {PlotEventClass},
		"Fields"	-> {
			"event_name" 	-> "lodend"
		}
	]
MetaEvent[LODEndClass]


(* ::Subsection:: *)
(* SelectionGeometryClass *)
	
SelectionGeometryClass = 
	NewClass[
		"Parents" 	-> {PlotEventClass},
		"Fields"	-> {
			"event_name" 	-> "selectiongeometry"
			, "geometry"	-> None
			, "finale"		-> True
		}
	]
MetaEvent[SelectionGeometryClass]


(* ::Subsection:: *)
(* ResetClass *)
	
ResetClass = 
	NewClass[
		"Parents" 	-> {PlotEventClass},
		"Fields"	-> {
			"event_name" 	-> "reset"
		}
	]
MetaEvent[ResetClass]


(* ::Subsection:: *)
(* PointEventClass *)
	
PointEventClass = 
	NewClass[
		"Parents" 	-> {PlotEventClass},
		"Fields"	-> {
			"sx"	-> None
			, "sy"	-> None
			, "x"	-> None
			, "y"	-> None
		}
	]

	
(* ::Subsection:: *)
(* TapEventClass *)
	
TapEventClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "tap"
		}
	]
MetaEvent[TapEventClass]


(* ::Subsection:: *)
(* DoubleTapClass *)
	
DoubleTapClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "doubletap"
		}
	]
MetaEvent[DoubleTapClass]


(* ::Subsection:: *)
(* PressClass *)
	
PressClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "press"
		}
	]
MetaEvent[PressClass]


(* ::Subsection:: *)
(* MouseEnterClass *)
	
MouseEnterClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "mouseenter"
		}
	]
MetaEvent[MouseEnterClass]


(* ::Subsection:: *)
(* MouseLeaveClass *)
	
MouseLeaveClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "mouseleave"
		}
	]
MetaEvent[MouseLeaveClass]


(* ::Subsection:: *)
(* MouseMoveClass *)
	
MouseMoveClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "mousemove"
		}
	]
MetaEvent[MouseMoveClass]


(* ::Subsection:: *)
(* MouseWheelClass *)
	
MouseWheelClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "wheel"
			, "delta"		-> None
		}
	]
MetaEvent[MouseWheelClass]


(* ::Subsection:: *)
(* PanClass *)
	
PanClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "pan"
			, "delta_x"		-> None
			, "delta_y"		-> None
			, "direction"	-> None
		}
	]
MetaEvent[PanClass]


(* ::Subsection:: *)
(* PanEndClass *)
	
PanEndClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "panend"
		}
	]
MetaEvent[PanEndClass]


(* ::Subsection:: *)
(* PanStartClass *)
	
PanStartClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "panstart"
		}
	]
MetaEvent[PanStartClass]


(* ::Subsection:: *)
(* PinchClass *)
	
PinchClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "pinch"
			, "scale"		-> None
		}
	]
MetaEvent[PinchClass]


(* ::Subsection:: *)
(* PinchEndClass *)
	
PinchEndClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "pinchend"
		}
	]
MetaEvent[PinchEndClass]


(* ::Subsection:: *)
(* PinchStartClass *)
	
PinchStartClass = 
	NewClass[
		"Parents" 	-> {PointEventClass},
		"Fields"	-> {
			"event_name"	-> "pinchstart"
		}
	]
MetaEvent[PinchStartClass]


(* ::Section:: *)
(* EventConstructorClass *)

EventConstructorClass /: 
	(p : EventConstructorClass[object_Symbol, ___]).(key : Except[sub | super | this, _Symbol | _String]) := 
	getAttributeMethod[p, key]

EventConstructorClass =
	NewClass[
		"Parents" 	-> {BaseClass},
		"Fields"	-> {}
	]
EventConstructorClass.getAllFunctions[] := Union@Flatten[
  GetFunctions[o.type[]][#] & /@ 
   Append[DeleteCases[Supers[o.type[]], BaseClass], o.type[]]]

EventConstructorClass.Event[opts___] 				:= New[EventClass][opts]
EventConstructorClass.ButtonClick[opts___] 			:= New[ButtonClickClass][opts]
EventConstructorClass.LODStart[opts___] 			:= New[LODStartClass][opts]
EventConstructorClass.LODEnd[opts___] 				:= New[LODEndClass][opts]
EventConstructorClass.SelectionGeometry[opts___]	:= New[SelectionGeometryClass][opts]
EventConstructorClass.Reset[opts___]				:= New[ResetClass][opts]
EventConstructorClass.TapEvent[opts___]				:= New[TapEventClass][opts]
EventConstructorClass.DoubleTap[opts___]			:= New[DoubleTapClass][opts]
EventConstructorClass.Press[opts___]				:= New[PressClass][opts]
EventConstructorClass.MouseEnter[opts___]			:= New[MouseEnterClass][opts]
EventConstructorClass.MouseLeave[opts___]			:= New[MouseLeaveClass][opts]
EventConstructorClass.MouseMove[opts___]			:= New[MouseMoveClass][opts]
EventConstructorClass.MouseWheel[opts___]			:= New[MouseWheelClass][opts]
EventConstructorClass.Pan[opts___]					:= New[PanClass][opts]
EventConstructorClass.PanEnd[opts___]				:= New[PanEndClass][opts]
EventConstructorClass.PanStart[opts___]				:= New[PanStartClass][opts]
EventConstructorClass.Pinch[opts___]				:= New[PinchClass][opts]
EventConstructorClass.PinchEnd[opts___]				:= New[PinchEndClass][opts]
EventConstructorClass.PinchStart[opts___]			:= New[PinchStartClass][opts]

Unprotect[events]
events = New[EventConstructorClass][]
SetAttributes[events, {ReadProtected, Protected}]

