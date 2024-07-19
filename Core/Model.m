(* Wolfram Language package *)

(* ::Section:: *)
(* ModelClass *)

ModelClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
			"name"						-> ""
			, "tags"					-> {}
			, "js_event_callbacks"		-> <||>
			, "subscribed_events"		-> {}
			, "js_property_callbacks"	-> <||>
			, "theme"					-> Null
		}
	]
	
ModelClass.init[opts : OptionsPattern[ModelClass] ] :=
	Module[{attrs},
		o.type = "Model";

		(* attribute fields *)
		attrs = o.attributes;
	]

ModelClass.select[arg_, ocontext_:Default] :=
	Module[{selector, context},
		selector = selectorHelper[arg];
		context = Switch[ocontext,
			None, None,
			_Association, ocontext,
			_, <|"plot"->o|>
		];
		(* calling references will evaluate attrs *)
		ListAttrSplat[find[o.references[], selector, context]]
	]

(* TODO: SetAttributes to hold all *)
ModelClass.jsonevent[event_, callbacks__] :=
	Module[{eventname=event},

		If[!StringQ[event] && IsInstanceQ[event, EventClass],
			eventname = event."event_name";
		];

		If[!KeyExistsQ[(o."js_event_callbacks"), eventname],
			SetAssociationField[o, "js_event_callbacks", eventname, {}]
		];
		
		Scan[(
			If[ !MemberQ[(o."js_event_callbacks")[eventname], #],
				SetAssociationField[o, "js_event_callbacks", eventname, 
					Append[(o."js_event_callbacks")[eventname], #]
					]
			]
		)&, {callbacks}];
	]


(* TODO: SetAttributes to hold all *)
(* Attach a CustomJS callback to an arbitrary BokehJS model event. *)
ModelClass.jsonchange[oevent_, ocallbacks___]:=
	Module[{event = oevent, callbacks = {ocallbacks}, cont},
		If[Length[callbacks] === 0,
			ValueError["jsonchange takes an event name and one or more callbacks, got only one parameter"];
			Return[]
		];
		
		cont = True;
		Scan[If[!IsInstanceQ[#, CustomJSClass],
				cont = False;
				ValueError["not all callback values are CustomJS instances"]							
		]&, callbacks];
		If[!cont, Return[]];

		If[MemberQ[o.getFields[], event], event="change:"<>event];
		
		If[!KeyExistsQ[(o."js_property_callbacks"), event],
			SetAssociationField[o, "js_property_callbacks", event, {}]
		];
		Scan[(
			If[ !MemberQ[(o."js_property_callbacks")[event], #],
				SetAssociationField[o, "js_property_callbacks", event, 
					Append[(o."js_property_callbacks")[event], #]
					]
			]
		)&, callbacks];
	]

(* TODO: SetAttributes to hold all *)
ModelClass.onchange[attr_, callbacks__] :=
	Module[{},
(*
        ''' Add a callback on this object to trigger when ``attr`` changes.

        Args:
            attr (str) : an attribute name on this object
            *callbacks (callable) : callback functions to register

        Returns:
            None

        Example:

        .. code-block:: python

            widget.on_change('value', callback1, callback2, ..., callback_n)

        '''
        if attr not in self.properties():
            raise ValueError("attempted to add a callback on nonexistent %s.%s property" % (self.__class__.__name__, attr))
        super(Model, self).on_change(attr, *callbacks)
*)
		None
	]
		
