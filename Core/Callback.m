(* Wolfram Language package *)


(*
	Model
		, CallbackClass
			, OpenURLClass
			, CustomJSClass
*)


(* ::Section:: *)
(* CallbackClass *)

(* abstract *)
CallbackClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			}
	]
	
CallbackClass.init[opts : OptionsPattern[CallbackClass] ] :=
	Module[{attrs},
		o.type	= "Callback";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* OpenURLClass *)

OpenURLClass =
	NewClass[
		"Parents"	-> {CallbackClass},
		"Fields"	-> {
				"url"	-> "BString"["http://"]
			}
	]
	
OpenURLClass.init[opts : OptionsPattern[OpenURLClass] ] :=
	Module[{attrs},
		o.type	= "OpenURL";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* CustomJSClass *)

CustomJSClass =
	NewClass[
		"Parents"	-> {CallbackClass},
		"Fields"	-> {
				"args"	-> "Dict"[<||>]
(*
    A mapping of names to Bokeh plot objects. These objects are made
    available to the callback code snippet as the values of named
    parameters to the callback.
*)				
				, "code"	-> "BString"[""]
(*
    A snippet of JavaScript code to execute in the browser. The
    code is made into the body of a function, and all of of the named objects in
    ``args`` are available as parameters that the code can use. Additionally,
    a ``cb_obj`` parameter contains the object that triggered the callback
    and an optional ``cb_data`` parameter that contains any tool-specific data
    (i.e. mouse coordinates and hovered glyph indices for the HoverTool).
*)
			}
	]
	
CustomJSClass.init[opts : OptionsPattern[CustomJSClass] ] :=
	Module[{attrs},
		o.type	= "CustomJS";

(*
		If[StringQ[o."code"], 
			o."code" = StringReplace[o."code", {
(*			
			"<"		-> "&lt;"
			"&"		-> "&amp;",
			"'"		-> "&apos;",
			"\""	-> "&quot;",
			">"		-> "&gt;",
			"\n"	-> "<br />"
			
			"'"->"\""
*)
			}]
		];
*)
		(* attribute fields *)
		attrs = o.attributes;
	]
