(* Wolfram Language Package *)

(* Copyright 2016-2018 by 1010data, Inc. Written by Ulises Cervantes-Pimentel *)

Global`PackageTimingStart[]

BeginPackage["MBokeh`", 
	{
		"MClasses`"
		, "MClasses`Core`MPlusPlus`"		
	}
]

(* Exported symbols added here with SymbolName::usage *)

(* ::Section:: *)
(* Unprotect and Clear symbols *)

$MBokehPublic = {
		  MBokeh`MImportBokehHTMLFile
		, MBokeh`MBokehWidgetShow
		, MBokeh`MBokehHTML
		, MBokeh`MBokehShow
		, MBokeh`MBokehStartWebDriver
		, MBokeh`MBokehStopWebDriver
		, MBokeh`MBokehCaptureScreen
		, MBokeh`InspectAttributes
		, MBokeh`DisplayJSONAttributes
		, MBokeh`show
}

(Unprotect[#]; ClearAll[#])& /@ $MBokehPublic

MImportBokehHTMLFile::usage	= "MImportBokehHTMLFile[name]."
MBokehWidgetShow::usage		= "MBokehWidgetShow[demoname]."
MBokehHTML::usage			= "MBokehHTML[demoname]."
MBokehShow::usage			= "MBokehShow[demoname]."
MBokehStartWebDriver::usage	= "MBokehStartWebDriver[]"
MBokehStopWebDriver::usage	= "MBokehStopWebDriver[]"
MBokehDisplayFunction::usage= "MBokehDisplayFunction[]"
MBokehCaptureScreen::usage	= "MBokehCaptureScreen[]"
show::usage					= "show[mode, type]"

Begin["`Private`"]

KOptions = Global`KOptions

(* Implementation of the package *)

(* ::Section:: *)
(* Globals *)

BokehVersionToNumeric[v_String] := Plus @@ ((ToExpression /@ Take[StringSplit[v, "."], -3])*{100,1,0.01})

$BokehVersion 	= "0.12.16"
$NBokehVersion 	= BokehVersionToNumeric[$BokehVersion]

filetemplate 	= Import["MBokeh/Templates/file.html"				, "Text"];
scripttemplate	= Import["MBokeh/Templates/script_tag.html"			, "Text"];
plotdivtemplate	= Import["MBokeh/Templates/plot_div.html"			, "Text"];
docjstemplate	= Import["MBokeh/Templates/doc_js.js"				, "Text"];

scripttemplate15= Import["MBokeh/Templates/script_tag_0_12_15.html"	, "Text"];
docjstemplate15	= Import["MBokeh/Templates/doc_js_0_12_15.js"		, "Text"];

widgetBokehHTML = "
    `plotdiv`
  
     `scripttag`
  ";

data = Partition[
   StringSplit[Import["MBokeh/Templates/demodata.txt"], "\n"], 4];
adata = Association[{ToExpression[#[[1]]] -> 
       Association[{"id" -> ToExpression[#[[2]]], "docs" -> #[[3]], 
         "render" -> #[[4]], "version"->$BokehVersion }]} & /@ data];

$GOOGLEAPI 		= Quiet@Import["MBokeh/Templates/gapi.txt", "Text"]
$GOOGLECLIENTID	= "gme-1010data"

If[!StringQ[$GOOGLEAPI], $GOOGLEAPI = None]

(*
        function loadGoogleScript() {
          // only load once, even if multiple gmaps on the page.
          if(jQuery('script').toArray()
               .filter(function(x){ return x.src.includes('maps.googleapis.com') })
               .length) {
            TenTen.App.hm_gmap();
          } else jQuery.getScript(
            'https://maps.googleapis.com/maps/api/js?'
            + '",:[""~apikey; "client=gme-1010data"; "key=",apikey],"'
            + '&libraries=visualization&v=3&callback=TenTen.App.hm_gmap');
        }
*)

(* ::Section:: *)
(* WebDriver *)

$WebDriverFirstRun	= True
$WebDriver 			= "WebDriver-Chrome"
$WebDriverHeadless	= "WebDriver-Chrome-Headless"

$WebSession 		= None
$WebSessionHeadless = None

$WindowPadding = {40,160}

$WebDriverQ := $WebDriverQ = ($VersionNumber>=11.3 && !$KEnvironmentQ)
$DisplayFuncOpts = {}

Options[MBokehStartWebDriver] = {
	"Headless" 		-> False,
	"WebDriver"		-> $WebDriver,
	ImageSize		-> {550,550},
	ImagePadding	-> Automatic
}

MBokehStartWebDriver[opts : OptionsPattern[] ] /; $WebDriverQ :=
	Module[{wsize, headless, webd, session, imgsize, imgpadding, qsize},
		headless 	= OptionValue["Headless"];
		webd 		= OptionValue["WebDriver"];
		imgsize		= OptionValue[ImageSize];
		imgpadding	= OptionValue[ImagePadding];

		If[MatchQ[imgpadding, {_Integer, _Integer}],
			$WindowPadding = imgpadding
		];
		
		qsize = MatchQ[imgsize, {_Integer, _Integer}];
		If[qsize, imgsize = imgsize + $WindowPadding];

		(* warm-up, needs to be reviewed in future mma versions > 11.3*)		
		If[$WebDriverFirstRun,
			session = Quiet@StartExternalSession["WebDriver-Chrome-Headless"];
			DeleteObject[session];
			$WebDriverFirstRun = False
		];
		
		If[!headless && $WebSession === None,
			$WebSession = StartExternalSession[webd];
			If[$WebSession === $Failed,
				$WebSession = None, (* else *)
				If[qsize,
					ExternalEvaluate[$WebSession, "SetWindowSize"->{imgsize}]
				];				
				wsize = ExternalEvaluate[$WebSession, "WindowSize"];
				Print[webd<>" "<>ToString[wsize]]
			];
			Return[$WebSession];
		];
		If[headless && $WebSessionHeadless === None,
			$WebSessionHeadless = StartExternalSession[webd<>"-Headless"];
			If[$WebSessionHeadless === $Failed, 
				$WebSessionHeadless = None, (* else *)
				If[qsize,
					ExternalEvaluate[$WebSessionHeadless, "SetWindowSize"->{imgsize}]
				];				
				wsize = ExternalEvaluate[$WebSessionHeadless, "WindowSize"];
				Print[webd<>"-Headless "<>ToString[wsize]]
				];
			Return[$WebSessionHeadless];
		];
	]
	
MBokehStopWebDriver[] /; $WebDriverQ :=
	Module[{},
		If[$WebSession =!= None, Quiet@DeleteObject[$WebSession]];
		If[$WebSessionHeadless =!= None, Quiet@DeleteObject[$WebSessionHeadless]];
		$WebSession = None;
		$WebSessionHeadless = None;
	]
MBokehCaptureScreen[] /; $WebDriverQ :=
	Module[{},
		If[$WebSessionHeadless =!= None,
		     Return[ExternalEvaluate[$WebSessionHeadless, "CaptureWebPage"] ]
		];
		If[$WebSession =!= None,
			 Return[ExternalEvaluate[$WebSession, "CaptureWebPage"] ]
		];
		None
	]

(* ::Section:: *)
(* Inspect Attributes *)

Clear[GetAttrs]
GetAttrs[name_String, fmt_: "JSON", version_:$BokehVersion] :=
	Module[{basic, res},
		basic = If[StringFreeQ[name, ".html"],
    			MBokeh`Private`adata[name],
    			MImportBokehHTMLFile[name, version]
    		];
    	If[!AssociationQ[basic], Return[$Failed]];
    	res = If[fmt === "JSON", 
    		ImportString[ToString@basic["docs"], "RawJSON"],
    		basic["docs"]
   		];
   		res
  ]

Clear[BGetTypeAttr]
BGetTypeAttr[refs_List, type_String, All] := Select[refs, (#["type"] === type &)]
BGetTypeAttr[refs_List, type_String] := 
	Module[{res}, 
		res = Select[refs, (#["type"] === type &)]; 
		If[Length[res] > 0, First@res, Null]
	]
BGetTypeAttr[name_String, type_String] := 
	BGetTypeAttr[GetAttrs[name][[1]]["roots"]["references"], type, All]	
	
Options[InspectAttributes] = {"version"->$BokehVersion, "depth"->4}
InspectAttributes[name_String, opts : OptionsPattern[] ] :=
	Module[{docs, refs, types, version, depth},
		version = OptionValue["version"];
		depth = OptionValue["depth"];
		docs = GetAttrs[name, "JSON", version];
		If[docs === $Failed, Return[$Failed]];
		refs = docs[[1]]["roots"]["references"];
		types = Sort@Union[#["type"] & /@ refs];
		
		Manipulate[ 
			Labeled[Framed[ExportString[BGetTypeAttr[#, type, All], "RawJSON", "Compact" -> depth] & /@ {refs}]
				, version, Top
			], 
			
			{type, types}
		]
  ]

DisplayJSONAttributes[name_String, depth_: 6, version_:$BokehVersion] :=
	Module[{docs},
		docs = GetAttrs[name, "JSON", version];
		If[docs === $Failed, Return[$Failed]];
		ExportString[docs, "RawJSON", "Compact" -> depth]
  ]

(* ::Section:: *)
(* MImportBokehHTMLFile *)

MImportBokehHTMLFile[fname_String, over: ("0.12.15" | "0.12.16")] := 
	Module[{fnameparts, script, vars, renderitems, tmp, xml, id, docs,
		render, docsjson, version, docid, ver},
		
		ver = StringReplace[over, "."->"_"];
		
		fnameparts = FileNameSplit[fname];
		xml = If[Length[fnameparts] === 1, Quiet@Import["MBokeh/HTMLExamples/" <> ver <> "/" <> fname, 
					"XMLObject"], Import[fname, "XMLObject"]];
					
		If[xml === $Failed, Return[$Failed]];
		script = First@Last@Cases[xml, XMLElement["script", {"type" -> "text/javascript"}, ___], Infinity][[All, 3]];
		vars = StringCases[script, Shortest["var " ~~ v : ___ ~~ " = " ~~ x : ___ ~~ ";"] -> {v, x}, Overlaps -> True];
		renderitems = Cases[vars, {"render_items", ___}];
		tmp = ImportString[renderitems[[1, 2]], "RawJSON"];
		id = tmp[[1]]["elementid"];
		render = ExportString[ImportString[renderitems[[1, 2]], "RawJSON"][[1]], "RawJSON", "Compact" -> 0];
		docsjson = Cases[vars, {"docs_json", ___}];
		docs = docsjson[[1, 2]];
		docid = First@StringCases[docs, "getElementById('" ~~ x___ ~~ "')" -> x];
		docs = Cases[xml, XMLElement["script", {"id" -> docid, ___}, ___], Infinity][[1, 3, 1]];
		tmp = First@Values@ImportString[docs, "RawJSON"];
		docs = StringCases[docs, "{" ~~ x___ ~~ "}" -> x];
		version = tmp["version"];
		<|"id" -> id, "docs" -> docs, "render" -> render, "version" -> version|>
   ]

MImportBokehHTMLFile[fname_String, ver_:$BokehVersion] :=
	Module[{fnameparts, script, vars, renderitems, tmp, xml, id, docs,render, docsjson, version},
		fnameparts = FileNameSplit[fname];
		xml = If[Length[fnameparts] === 1,
    		Quiet@Import["MBokeh/HTMLExamples/" <> fname, "XMLObject"],
    		Import[fname, "XMLObject"]
    	];
    	If[xml === $Failed, Return[$Failed]];
    	script = First@Last@Cases[xml,
    				XMLElement["script", {"type" -> "text/javascript"}, ___],Infinity][[All, 3]];
    	vars = StringCases[script, Shortest["var " ~~ v : ___ ~~ " = " ~~ x : ___ ~~ ";"] -> {v, x}, Overlaps -> True];
    	renderitems = Cases[vars, {"render_items", ___}];
    	tmp = ImportString[renderitems[[1, 2]], "RawJSON"];
    	id = tmp[[1]]["elementid"];
    	render = ExportString[
    				ImportString[renderitems[[1, 2]], "RawJSON"][[1]], "RawJSON", "Compact" -> 0];
    	docsjson = Cases[vars, {"docs_json", ___}];
    	docs = docsjson[[1, 2]];
    	tmp = First@Values@ImportString[docs,"RawJSON"];
    	version = tmp["version"];
    	<|"id" -> id, "docs" -> docs, "render" -> render, "version"->version|>
  ]


(* ::Section:: *)
(* MBokehWidgetShow *)

MBokehWidgetShow[demo:(_String | _Association), version_:$BokehVersion] := 
	Module[{script, plotdiv, html, docsjs, data, id, docs, render, 
 		doctmpl, scripttmpl, nversion, scriptid, assoc},
 		
 		data = Switch[demo,
  			_Association, demo, 
  			_String, adata[demo],
  			_, $Failed
  			];
  		If[!AssociationQ[data] && StringQ[demo],
  			data = MImportBokehHTMLFile[demo, version] ];
  		If[!AssociationQ[data], Return[$Failed]];

  		nversion = BokehVersionToNumeric[version];

  		doctmpl 	= If[nversion>=12.15, docjstemplate15, docjstemplate ];
  		scripttmpl	= If[nversion>=12.15, scripttemplate15, scripttemplate];

		scriptid = CreateUUID[];
		id 		= data["id"];
		docs 	= data["docs"];
		render 	= data["render"];
		
		plotdiv = StringTemplate[plotdivtemplate][<|"elementid" -> id|>];
		
		assoc = If[nversion>=12.15,
			<|"render_items" -> render, "script_id"->scriptid|>,
			<|"docs_json" -> docs, "render_items" -> render|>
			];
		docsjs = StringTemplate[doctmpl][assoc];
  
		assoc = If[nversion>=12.15, 
			<|"docs_json" -> docs, "js_code" -> docsjs, "script_id"->scriptid|>,
			<|"js_code" -> docsjs|>];
		script = StringTemplate[scripttmpl][assoc];
		
		html = StringTemplate[widgetBokehHTML][<|"plotdiv" -> plotdiv, "scripttag" -> script|>];
		
		html
  ]

  
MBokehWidgetShow[gr_Graphics, version_:$BokehVersion] := MBokehWidgetShow["bar_basic", version]

(* ::Section:: *)
(* MBokehHTML *)

MBokehHTML[demo_, version_:$BokehVersion] := Module[{html, body, title},
  title = If[StringQ[demo], demo, "Mathematica Bokeh Plot"];
  body = MBokehWidgetShow[demo, version];
  html = StringTemplate[filetemplate][
    Association[{"bversion"->version, "title" -> title, "plotbody" -> body}]];
  html
  ]

(* ::Section:: *)
(* MBokehHTML *)

Options[MBokehShow] = {
	ImageSize 	-> Automatic,
	"NewPage"	-> False
}

MBokehShow[gr:(_Graphics | _Graphics3D), opts___ ] := 
	Module[{ff},
		ff = MBokehDisplayFunction[];
		If[ff === Identity,
			Show[gr, opts],
			(* else *)
			$DisplayFuncOpts = {opts};
			Show[gr, DisplayFunction -> ff ]
		]
	]

MBokehShow[demo_, version_:$BokehVersion, opts___] := 
	Module[{html, file, filename, webimg = None, webpage, imgsize, qsize, ff},
		filename = If[StringQ[demo], 
						demo,
						FileNameJoin[{$TemporaryDirectory, "mbokeh"}]
					];
		html = MBokehHTML[demo, version];
		file = Export[filename <> ".html", html, "Text"];
		If[!FileExistsQ[file], Return[$Failed]];
		ff = MBokehDisplayFunction[]; 
		If[ ff === Identity, (* then *)
			SystemOpen[file];
			Return[file], (* else *)
			
			imgsize = ImageSize /. opts;
			qsize = MatchQ[imgsize, {_Integer,_Integer}];
			If[qsize, imgsize = imgsize + $WindowPadding ];

			webpage = StringReplace[file, {"\\" -> "/"}];
			If[$WebSessionHeadless =!= None,
				If[qsize,
					ExternalEvaluate[$WebSessionHeadless, "SetWindowSize" -> {imgsize}]
				];
				ExternalEvaluate[$WebSessionHeadless, "OpenWebPage" -> webpage];
		     	webimg = ExternalEvaluate[$WebSessionHeadless, "CaptureWebPage"];
			];
			If[$WebSession =!= None,
				If[qsize,
					ExternalEvaluate[$WebSession, "SetWindowSize" -> {imgsize}]
				];
				ExternalEvaluate[$WebSession, "OpenWebPage" -> webpage];
				If[webimg === None,
			     	webimg = ExternalEvaluate[$WebSession, "CaptureWebPage"]					
				]
			];
	     	webimg
		]
  ]


(* ::Section:: *)
(* Create Document *)

Clear[BCreateDocument]
Options[BCreateDocument] = 
	{
		"Title"			-> "1010data Application",
		"RootIDS"		-> Null,
		"References" 	-> None
	}
BCreateDocument[opts : OptionsPattern[] ] := 
 Module[{docid, roots, title, version, references, rootids, bdocs, refs, basic},
  	title 	= OptionValue["Title"];
 	refs 	= OptionValue["References"];
 	rootids	= OptionValue["RootIDS"];
  	version = $BokehVersion;

	references = refs;
	If[!ListQ[references] || Length[references]===0,
  		basic = adata["bar_basic"];
  		bdocs = ImportString[ToString@basic["docs"], "RawJSON"];
  		references = bdocs[[1]]["roots"]["references"]
	];
	If[rootids === Null,
	  	rootids = Cases[{#["type"], #["id"]} & /@ references, {"Plot", _}][[All, 2]];
	];
  
  	roots = <|"references" -> references, "root_ids" -> rootids|>;	
  
  	docid = BokehGetID[];
  	<|docid -> <|"roots" -> roots, "title" -> title, "version" -> version|>|>
  ]


Clear[BCreateJSON]
BCreateJSON[docs_Association] := 
	Module[{res, id, render, ndocs, nrender, docid},
		id = BokehGetID[];
		docid = First[Keys[docs]];
		render = <|
    		"docid" -> docid,
    		"elementid" -> id,
    		"modelid" -> First@docs[docid]["roots"]["root_ids"]
    	|>;
    	nrender = ExportString[render, "RawJSON", "Compact" -> 0];
    	ndocs = ExportString[docs, "RawJSON", "Compact" -> 0];
  
  		res = <|"id" -> id, "docs" -> ndocs, "render" -> nrender|>
  ]

(* ::Section:: *)
(* show *)

KEmbeddedHTML	:= KEmbeddedHTML	= TenTenData`TenTenAPI`KEmbeddedHTML

Options[show] = Options[MBokehShow]

show[e_, opts : OptionsPattern[] ] /; $KEnvironmentQ := KEmbeddedHTML[e, "Bokeh"]

show[gr:(_Graphics | _Graphics3D), opts : OptionsPattern[] ] := MBokehShow[gr, opts]


show[root_, type:("WEB" | "HTML" | "Widget"), version_:$BokehVersion, opts : OptionsPattern[] ] :=
	Module[{refs, docs, json},
		refs = root.serializeInstances[];
		If[!ListQ[refs], Return[$Failed] ];
		docs = BCreateDocument["References" -> refs, "RootIDS"-> {root."id"}];
		json = BCreateJSON[docs];
		Switch[type,
			"WEB"	, MBokehShow[json, version, opts],
			"HTML"	, MBokehHTML[json, version],
			"Widget", MBokehWidgetShow[json, version]
		]
	]

show[root_, opts : OptionsPattern[] ] := 
	Module[{isize, nopts, padding},
		nopts = {opts};
		isize = OptionValue[ImageSize];
		If[isize === Automatic,
			padding = If[!$CloudEvaluation, {0,0}, {150,100}];
			nopts = 
				Switch[root."type",
					"Plot",
						Prepend[{opts}, 
							Rule[ImageSize,
								{(root."plot_width")+padding[[1]], (root."plot_height")+padding[[2]]}]],
					_, {opts}
				]
		];
		If[!$CloudEvaluation,
			show[root, "WEB", $BokehVersion, Sequence@@nopts],
			EmbeddedHTML[show[root, "HTML"], Sequence@@nopts]
		]
	]

End[]

(* ::Section:: *)
(* Protect Symbols *)

SetAttributes[{#}, {ReadProtected, Protected}]& /@ $MBokehPublic
Remove[$MBokehPublic]

EndPackage[]

Global`PackageTimingEnd[]
