(* Wolfram Language package *)

(*

				, WidgetClass
					, PanelWidgetClass
					, TabsClass
					, MarkupClass
						, ParagraphClass
						, DivWidgetClass
						, PreTextClass
					, AbstractIconClass
					, AbstractGroupClass
						, ButtonGroupClass
							, RadioGroupClass
							, CheckboxButtonGroupClass
						, GroupClass
							, CheckboxGroupClass
						, RadioButtonGroupClass
					, InputWidgetClass
						, TextInputClass
							, PasswordInputClass
							, AutocompleteInputClass
						, SelectWidgetClass
						, MultiSelectClass
						, DatePickerClass
					, ButtonLikeClass
						, AbstractButtonClass
							, ButtonWidgetClass
							, ToggleWidgetClass
							, DropdownWidgetClass				
					, AbstractSliderClass
						, SliderWidgetClass
						, RangeSliderClass
						, DateSliderClass
						, DateRangeSliderClass

*)


(* ::Subsection:: *)
(* PanelWidgetClass *)

PanelWidgetClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"title"		-> "BString"[""]
			, "child"	-> "Instance"[LayoutDOMClass]
			, "closable"	-> "BBool"[False]
		}
	]
	
PanelWidgetClass.init[opts : OptionsPattern[PanelWidgetClass] ] :=
	Module[{attrs},
		o.type = "Panel";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* TabsClass *)

TabsClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"tabs"		-> List[PanelWidgetClass]
			, "active"	-> "BInt"[0]
			, "callback"	-> "Instance"[CallbackClass]
		}
	]
	
TabsClass.init[opts : OptionsPattern[TabsClass] ] :=
	Module[{attrs},
		o.type = "Tabs";

		If[o."callback" === CallbackClass, o."callback" = Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MarkupClass *)

(*
	Base class for Bokeh models that represent HTML markup elements.
    Markups include e.g., ``<div>``, ``<p>``, and ``<pre>``.
*)

MarkupClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"text"		-> "BString"[""]
			, "style"		-> <||>	(*	Dict(String, Any, default={}, *)
(*			
    Raw CSS style declaration. Note this may be web browser dependent.
*)		}
	]
	
MarkupClass.init[opts : OptionsPattern[MarkupClass] ] :=
	Module[{attrs},
		o.type = "Markup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ParagraphClass *)

ParagraphClass =
	NewClass[
		"Parents"		-> {MarkupClass},
		"Fields"		-> {
		}
	]
	
ParagraphClass.init[opts : OptionsPattern[ParagraphClass] ] :=
	Module[{attrs},
		o.type = "Paragraph";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DivWidgetClass *)

DivWidgetClass =
	NewClass[
		"Parents"		-> {MarkupClass},
		"Fields"		-> {
			"render_as_text"	-> "BBool"[False]
		}
	]
	
DivWidgetClass.init[opts : OptionsPattern[DivWidgetClass] ] :=
	Module[{attrs},
		o.type = "Div";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* PreTextClass *)

PreTextClass =
	NewClass[
		"Parents"		-> {ParagraphClass},
		"Fields"		-> {
		}
	]
	
PreTextClass.init[opts : OptionsPattern[PreTextClass] ] :=
	Module[{attrs},
		o.type = "PreText";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* AbstractIconClass *)

AbstractIconClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
		}
	]
	
AbstractIconClass.init[opts : OptionsPattern[AbstractIconClass] ] :=
	Module[{attrs},
		o.type = "AbstractIcon";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* AbstractGroupClass *)

AbstractGroupClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"labels"		-> {}
			, "callback"		-> "Instance"[CallbackClass]
		}
	]
	
AbstractGroupClass.init[opts : OptionsPattern[AbstractGroupClass] ] :=
	Module[{attrs},
		o.type = "AbstractGroup";

		If[o."callback" === CallbackClass, o."callback"=Null];

		(* attribute fields *)
		attrs = o.attributes;
	]
AbstractGroupClass.onclick[handler_] :=
	Module[{},
	(*
    def on_click(self, handler):
        ''' Set up a handler for button check/radio box clicks including
        the selected indices.

        Args:
            handler (func) : handler function to call when button is clicked.

        Returns:
            None

        '''
        self.on_change('active', lambda attr, old, new: handler(new))
	*)
		Null		
	]
	
AbstractGroupClass.jsonclick[handler_] :=
	Module[{},
        o.jsonchange["active", handler]		
	]
	

(* ::Subsection:: *)
(* ButtonGroupClass *)

ButtonGroupClass =
	NewClass[
		"Parents"		-> {AbstractGroupClass, ButtonLikeClass},
		"Fields"		-> {
		}
	]
	
ButtonGroupClass.init[opts : OptionsPattern[ButtonGroupClass] ] :=
	Module[{attrs},
		o.type = "ButtonGroup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* GroupClass *)

GroupClass =
	NewClass[
		"Parents"		-> {AbstractGroupClass},
		"Fields"		-> {
			"inline"	-> "BBool"[False]
		}
	]
	
GroupClass.init[opts : OptionsPattern[GroupClass] ] :=
	Module[{attrs},
		o.type = "Group";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CheckboxGroupClass *)

CheckboxGroupClass =
	NewClass[
		"Parents"		-> {GroupClass},
		"Fields"		-> {
			"active"	-> {}	(* List(Int, *)
		}
	]
	
CheckboxGroupClass.init[opts : OptionsPattern[CheckboxGroupClass] ] :=
	Module[{attrs},
		o.type = "CheckboxGroup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* RadioGroupClass *)

RadioGroupClass =
	NewClass[
		"Parents"		-> {GroupClass},
		"Fields"		-> {
			"active"	-> "BInt"[None]
		}
	]
	
RadioGroupClass.init[opts : OptionsPattern[RadioGroupClass] ] :=
	Module[{attrs},
		o.type = "RadioGroup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CheckboxButtonGroupClass *)

CheckboxButtonGroupClass =
	NewClass[
		"Parents"		-> {ButtonGroupClass},
		"Fields"		-> {
			"active"	-> {}	(* List(Int, *)
		}
	]
	
CheckboxButtonGroupClass.init[opts : OptionsPattern[CheckboxButtonGroupClass] ] :=
	Module[{attrs},
		o.type = "CheckboxButtonGroup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* RadioButtonGroupClass *)

RadioButtonGroupClass =
	NewClass[
		"Parents"		-> {ButtonGroupClass},
		"Fields"		-> {
			"active"		-> "BInt"[None]
		}
	]
	
RadioButtonGroupClass.init[opts : OptionsPattern[RadioButtonGroupClass] ] :=
	Module[{attrs},
		o.type = "RadioButtonGroup";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* InputWidgetClass *)

InputWidgetClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"title"		-> "BString"[""]
		}
	]
	
InputWidgetClass.init[opts : OptionsPattern[InputWidgetClass] ] :=
	Module[{attrs},
		o.type = "InputWidget";

		(* attribute fields *)
		attrs = o.attributes;
	]
InputWidgetClass.coarcevalue[cls_, oval_] := 
	Module[{val = oval},
(*
    def coerce_value(cls, val):
        prop_obj = cls.lookup('value')
        if isinstance(prop_obj, Float):
            return float(val)
        elif isinstance(prop_obj, Int):
            return int(val)
        elif isinstance(prop_obj, String):
            return str(val)
        else:
            return val
*)
			val
	]


(* ::Subsection:: *)
(* TextInputClass *)

TextInputClass =
	NewClass[
		"Parents"		-> {InputWidgetClass},
		"Fields"		-> {
			"value"			-> "BString"[""]
			, "callback"	-> "Instance"[CallbackClass]
			, "placeholder"	-> "BString"[""]
		}
	]
	
TextInputClass.init[opts : OptionsPattern[TextInputClass] ] :=
	Module[{attrs},
		o.type = "TextInput";
		
		If[o."callback" === CallbackClass, o."callback"=Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* PasswordInputClass *)

PasswordInputClass =
	NewClass[
		"Parents"		-> {TextInputClass},
		"Fields"		-> {
		}
	]
	
PasswordInputClass.init[opts : OptionsPattern[PasswordInputClass] ] :=
	Module[{attrs},
		o.type = "PasswordInput";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* AutocompleteInputClass *)

AutocompleteInputClass =
	NewClass[
		"Parents"		-> {TextInputClass},
		"Fields"		-> {
			"completions"	-> {}	(* List(String, *)
		}
	]
	
AutocompleteInputClass.init[opts : OptionsPattern[AutocompleteInputClass] ] :=
	Module[{attrs},
		o.type = "AutocompleteInput";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* SelectWidgetClass *)

SelectWidgetClass =
	NewClass[
		"Parents"		-> {InputWidgetClass},
		"Fields"		-> {
			"options"		-> 	"Either"[Null]	(* = Either( List(Either(String, Tuple(Either(Int,String), String))),
        Dict(String, List(Either(String, Tuple(Either(Int,String), String)))), *)
(*        
    Available selection options. Options may be provided either as a list of
    possible string values, or as a list of tuples, each of the form
    ``(value, label)``. In the latter case, the visible widget text for each
    value will be corresponding given label. Option groupings can be provided
    by supplying a dictionary object whose values are in the aforementioned
    list format
*)
			, "value"		-> "BString"[""]
			, "callback"	-> "Instance"[CallbackClass]
		}
	]
	
SelectWidgetClass.init[opts : OptionsPattern[SelectWidgetClass] ] :=
	Module[{attrs},
		o.type = "Select";

		If[o."callback" === CallbackClass, o."callback"=Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* MultiSelectClass *)

MultiSelectClass =
	NewClass[
		"Parents"		-> {InputWidgetClass},
		"Fields"		-> {
			"options"		-> {}	(* List(Either(String, Tuple(String, String)), *)
(*			
    Available selection options. Options may be provided either as a list of
    possible string values, or as a list of tuples, each of the form
    ``(value, label)``. In the latter case, the visible widget text for each
    value will be corresponding given label.
*)
			, "value"			-> {}	(* List(String, *)
			, "callback"		-> "Instance"[CallbackClass]
			, "size"			-> "BInt"[4]
		}
	]
	
MultiSelectClass.init[opts : OptionsPattern[MultiSelectClass] ] :=
	Module[{attrs},
		o.type = "MultiSelect";

		If[o."callback" === CallbackClass, o."callback"=Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* DatePickerClass *)

DatePickerClass =
	NewClass[
		"Parents"		-> {InputWidgetClass},
		"Fields"		-> {
			"value"			-> "BDate"[Null]
			, "min_date"	-> "BDate"[None]
			, "max_date"	-> "BDate"[None]
			, "callback"	-> "Instance"[CallbackClass]
		}
	]
	
DatePickerClass.init[opts : OptionsPattern[DatePickerClass] ] :=
	Module[{attrs},
		o.type = "DatePicker";

		If[o."callback" === CallbackClass, o."callback"=Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* ButtonLikeClass *)


ButtonLikeClass =
	NewClass[
		"Parents"		-> {BokehAttributeClass},
		"Fields"		-> {
			"button_type"	-> "Enum"["default", EnumButtonType]
		}
	]
	
ButtonLikeClass.init[opts : OptionsPattern[ButtonLikeClass] ] :=
	Module[{attrs},
		o.type = "ButtonLike";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* AbstractButtonClass *)


AbstractButtonClass =
	NewClass[
		"Parents"		-> {WidgetClass, ButtonLikeClass},
		"Fields"		-> {
			"label"		-> "BString"[""]
			, "icon"		-> "Instance"[AbstractIconClass]
			, "callback"	-> "Instance"[CallbackClass]
		}
	]
	
AbstractButtonClass.init[opts : OptionsPattern[AbstractButtonClass] ] :=
	Module[{attrs},
		o.type = "AbstractButton";

		If[o."label" === "", o."label" = "Button"];
		If[o."callback" === CallbackClass		, o.callback = Null];
		If[o."icon" 	=== AbstractIconClass	, o.icon = Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ButtonWidgetClass *)

ButtonWidgetClass =
	NewClass[
		"Parents"		-> {AbstractButtonClass},
		"Fields"		-> {
			"clicks"		-> "BInt"[0]
		}
	]
	
ButtonWidgetClass.init[opts : OptionsPattern[ButtonWidgetClass] ] :=
	Module[{attrs},
		o.type = "Button";

		(* attribute fields *)
		attrs = o.attributes;
	]

ButtonWidgetClass.onclick[handler_] :=
	Module[{},
(*
    def on_click(self, handler):
        ''' Set up a handler for button clicks.

        Args:
            handler (func) : handler function to call when button is clicked.

        Returns:
            None

        '''
        self.on_change('clicks', lambda attr, old, new: handler())
*)				
		Null
	]

ButtonWidgetClass.jsonclick[handler_] :=
	Module[{},
		o.jsonchange["clicks", handler];
		None
	]


(* ::Subsubsection:: *)
(* ToggleWidgetClass *)


ToggleWidgetClass =
	NewClass[
		"Parents"		-> {AbstractButtonClass},
		"Fields"		-> {
			"label"		-> "BString"[""]
			, "active"	-> "BBool"[False]
		}
	]
	
ToggleWidgetClass.init[opts : OptionsPattern[ToggleWidgetClass] ] :=
	Module[{attrs},
		o.type = "Toggle";

		If[o."label" === "", o."label" = "Toggle"];

		(* attribute fields *)
		attrs = o.attributes;
	]

ToggleWidgetClass.onclick[handler_] :=
	Module[{},
(*
    def on_click(self, handler):
        """ Set up a handler for button state changes (clicks).

        Args:
            handler (func) : handler function to call when button is toggled.

        Returns:
            None

        """
        self.on_change('active', lambda attr, old, new: handler(new))
*)
		None		
	]

ToggleWidgetClass.jsonclick[handler_] :=
	Module[{},
		o.jsonchange["active", handler];
		None		
	]


(* ::Subsubsection:: *)
(* DropdownWidgetClass *)

DropdownWidgetClass =
	NewClass[
		"Parents"		-> {AbstractButtonClass},
		"Fields"		-> {
			"label"				-> "BString"[""]
			, "value"			-> "BString"[""]
			, "default_value"	-> "BString"[""]
			, "menu"			-> {}	(* List(Tuple(String, String), *)
(*
    Button's dropdown menu consisting of entries containing item's text and
    value name. Use ``None`` as a menu separator.
*)
		}
	]
	
DropdownWidgetClass.init[opts : OptionsPattern[DropdownWidgetClass] ] :=
	Module[{attrs},
		o.type = "Dropdown";

		If[o."label" === "", o."label" = "Dropdown"];
		If[ListQ[o."menu"], o."menu" = o."menu" /. None->Null];

		(* attribute fields *)
		attrs = o.attributes;
	]

DropdownWidgetClass.onclick[handler_] :=
	Module[{},
(*
        ''' Set up a handler for button or menu item clicks.

        Args:
            handler (func) : handler function to call when button is activated.

        Returns:
            None

        '''
        self.on_change('value', lambda attr, old, new: handler(new))
*)
		None		
	]

DropdownWidgetClass.jsonclick[handler_] :=
	Module[{},
		o.jsonchange["value", handler];
		None		
	]


(* ::Section:: *)
(* AbstractSliderClass *)

AbstractSliderClass =
	NewClass[
		"Parents"		-> {WidgetClass},
		"Fields"		-> {
			"title"					-> "BString"[""]
			, "show_value"			-> "BBool"[True]
			, "format"				-> "BString"[""]
			, "orientation"			-> "Enum"["horizontal", {"horizontal", "vertical"}]
			, "direction"			-> "Enum"["ltr"], {"ltr","rtl"}
			, "tooltips"			-> "BBool"[True]
			, "callback"			-> "Instance"[CallbackClass]
			, "callback_throttle"	-> "BFloat"[200]
			, "callback_policy"	 	-> "Enum"["throttle", EnumSliderCallbackPolicy]
(*			
    When the callback is initiated. This parameter can take on only one of three options:

    * "continuous": the callback will be executed immediately for each movement of the slider
    * "throttle": the callback will be executed at most every ``callback_throttle`` milliseconds.
    * "mouseup": the callback will be executed only once when the slider is released.

    The "mouseup" policy is intended for scenarios in which the callback is expensive in time.
*)
			, "bar_color"			-> "BColor"["#e6e6e6"]
		}
	]
	
AbstractSliderClass.init[opts : OptionsPattern[AbstractSliderClass] ] :=
	Module[{attrs},
		o.type = "AbstractSlider";

		If[o."callback" === CallbackClass		, o.callback = Null];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* SliderWidgetClass *)

SliderWidgetClass =
	NewClass[
		"Parents"		-> {AbstractSliderClass},
		"Fields"		-> {
			"start"		-> "BFloat"[Null]
			, "end"		-> "BFloat"[Null]
			, "value"	-> "BFloat"[Null]
			, "step"	-> "BFloat"[1]
			, "format"	-> "BFloat"["0[.]00"]	(* Override(default="0[.]00") *)
		}
	]
	
SliderWidgetClass.init[opts : OptionsPattern[SliderWidgetClass] ] :=
	Module[{attrs},
		o.type = "Slider";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* RangeSliderClass *)

RangeSliderClass =
	NewClass[
		"Parents"		-> {AbstractSliderClass},
		"Fields"		-> {
			"value"		-> {}	(* Tuple(Float, Float, *)
			, "start"	-> "BFloat"[Null]
			, "end"		-> "BFloat"[Null]
			, "step"	-> "BFloat"[1]
			, "format"	-> "BFloat"["0[.]00"]	(* Override(default="0[.]00") *)
		}
	]
	
RangeSliderClass.init[opts : OptionsPattern[RangeSliderClass] ] :=
	Module[{attrs},
		o.type = "RangeSlider";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* DateSliderClass *)

DateSliderClass =
	NewClass[
		"Parents"		-> {AbstractSliderClass},
		"Fields"		-> {
			"value"		-> "BDate"[Null]
			, "start"	-> "BDate"[Null]
			, "end"		-> "BDate"[Null]
			, "step"	-> "BInt"[1]
			, "format"	-> "BString"["%d %b %Y"]	(*Override(default="%d %b %Y") *)
		}
	]
	
DateSliderClass.init[opts : OptionsPattern[DateSliderClass] ] :=
	Module[{attrs},
		o.type = "DateSlider";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* DateRangeSliderClass *)

DateRangeSliderClass =
	NewClass[
		"Parents"		-> {AbstractSliderClass},
		"Fields"		-> {
			"value"		-> {}	(* Tuple(Date, Date, *)
			, "start"	-> "BDate"[Null]
			, "end"		-> "BDate"[Null]
			, "step"	-> "BInt"[1]
			, "format"	-> "BString"["%d %b %Y"] 	(* Override(default="%d %b %Y") *)
		}
	]
	
DateRangeSliderClass.init[opts : OptionsPattern[DateRangeSliderClass] ] :=
	Module[{attrs},
		o.type = "DateRangeSlider";

		(* attribute fields *)
		attrs = o.attributes;
	]

DateRangeSliderClass.valueasdatetime[] :=
	Module[{},
		If[MatchQ[o."value", (Null | None)], Return[None]];
		(* TODO: Finish implementing *)
		o."value"
	]

	
	