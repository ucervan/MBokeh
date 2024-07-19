(* Wolfram Language package *)

(*
	ModelClass
		, ExpressionModelClass
			, StackModelClass
		, SelectionModelClass
		, SelectionPolicyClass
			, IntersectRenderersClass
			, UnionRenderersClass
		, DataSourceClass
			, ColumnarDataSourceClass
				, ColumnDataSourceClass
					, RemoteSourceClass
						, AjaxDataSourceClass
				, GeoJSONDataSourceClass
		, CDSViewClass
*)


(* ::Section:: *)
(* ToUInteger32 *)

Options[ToUInteger32] = {RasterSize->400}

$cTarget = If[$VersionNumber >= 11.2, "C", "WVM"]

touint32 = Quiet@Compile[{{carray, _Real, 2}, {crange, _Real, 1}, {ff, _Integer}},
   Module[{rgbarray, min, max, xdim, ydim, xrange, yrange, col, cdif, 
     val, val255},
    	{min, max} = crange;
    	cdif = max - min;
    	{xdim, ydim} = Dimensions[carray];
    	xrange = Range[xdim];
    	yrange = Range[ydim];
    
    	rgbarray = Developer`ToPackedArray@Table[0, {xdim}, {ydim}];
		val255 = BitShiftLeft[255, 24];
    	If[cdif > 0.,
     	Scan[(col = #;
        	Scan[(
				val = Floor[255 * (carray[[col, #]] - min)/cdif];
				rgbarray[[col, #]] = val255 + BitShiftLeft[val, 16] + BitShiftLeft[val, 8] + val
            	) &, yrange]
        	) &
      	, xrange], (* else, do not rescale *)
     	Scan[(col = #;
        	Scan[(
				val = Floor[255 * carray[[col, #]]];
				rgbarray[[col, #]] = val255 + BitShiftLeft[val, 16] + BitShiftLeft[val, 8] + val
            	) &, yrange]
        	) &
      	, xrange]
     	];
    	rgbarray
    ], CompilationTarget -> $cTarget]
    
ToUInteger32[a_] /; ArrayQ[a] && Length[Dimensions[a]]===2 := touint32[a, {0.,0.}, 255]
ToUInteger32[a_, {min_,max_}] /; ArrayQ[a] && Length[Dimensions[a]]===2 := touint32[a, {min,max}, 255]

ToUInteger32[a_List] := FromDigits[Join @@ (Rest@IntegerDigits[# + 256, 2] & /@ a), 2]

v255 = BitShiftLeft[255, 24]
ToUInteger32[im_Image] /; ImageChannels[im] === 1 := 
	Module[{v},
		(Reverse@ImageData[im]) /. g_Real -> (v=Floor[255 g];
 			v + BitShiftLeft[v, 16] + BitShiftLeft[v, 8] + v)
	]
ToUInteger32[im_Image] /; ImageChannels[im] === 3 :=
		(Reverse@ImageData[im]) /. {r_Real, g_, b_} -> 
 			v255 + BitShiftLeft[Floor[255 b], 16] + BitShiftLeft[Floor[255 g], 8] + Floor[255 r]

ToUInteger32[im_Image] /; ImageChannels[im] === 4 := 
	(Reverse@ImageData[im]) /. {r_, g_, b_, a_} -> 
 		BitShiftLeft[Floor[255 a], 24] + BitShiftLeft[Floor[255 b], 16] + 
 			BitShiftLeft[Floor[255 g], 8] + Floor[255 r]

ToUInteger32[Graphics[Raster[data_, rest___], ___]] /; MatchQ[Dimensions[data],{_,_,3}] :=
		(data) /. {r_Integer, g_, b_} -> 
 			v255 + BitShiftLeft[b, 16] + BitShiftLeft[g, 8] + r

ToUInteger32[Graphics[Raster[data_, rest___], ___]] /; MatchQ[Dimensions[data],{_,_,4}] :=
		(data) /. {a_, r_Integer, g_, b_} -> 
 			BitShiftLeft[a,24] + BitShiftLeft[b, 16] + BitShiftLeft[g, 8] + r

(*

to3uint32 = Quiet@Compile[{{carray, _Real, 3}, {crange, _Real, 1}, {ff, _Integer}},
   Module[{rgbarray, min, max, xdim, ydim, xrange, yrange, col, cdif, 
     rval, gval, bval, val255, zdim, r, g, b},
    	{min, max} = crange;
    	cdif = max - min;
    	{xdim, ydim, zdim} = Dimensions[carray];
    	If[zdim =!= 3, Return[-1]];
    	xrange = Range[xdim];
    	yrange = Range[ydim];
    
    	rgbarray = Developer`ToPackedArray@Table[0, {xdim}, {ydim}];
    	val255 = BitShiftLeft[255, 24];
    	If[cdif > 0.,
     	Scan[(col = #;
        	Scan[(
        		{r,g,b} = carray[[col,#]];
           		rval = Floor[ff * (r - min)/cdif];
           		gval = BitShiftLeft[Floor[ff * (g - min)/cdif], 8];
           		bval = BitShiftLeft[Floor[ff * (b - min)/cdif], 16];
           		rgbarray[[col, #]] = val255+bval+gval+rval
           	) &, yrange]
        	) &
      	, xrange], (* else not rescale *)
     	Scan[(col = #;
        	Scan[(           	
        		{r,g,b} = carray[[col,#]];
           		rval = Floor[ff * r ];
           		gval = BitShiftLeft[Floor[ff * g], 8];
           		bval = BitShiftLeft[Floor[ff * b], 16];
           		rgbarray[[col, #]] = val255 + bval +gval + rval
           	) &, yrange]
        	) &
      	, xrange]
     	];
    	rgbarray
    ], CompilationTarget -> $cTarget]
    
to4uint32 = Quiet@Compile[{{carray, _Real, 3}, {crange, _Real, 1}, {ff, _Integer}},
   Module[{rgbarray, min, max, xdim, ydim, xrange, yrange, col, cdif, 
     rval, gval, bval, aval, zdim, r,g,b,a},
    	{min, max} = crange;
    	cdif = max - min;
    	{xdim, ydim, zdim} = Dimensions[carray];
    	If[zdim =!= 4, Return[-1]];
    	xrange = Range[xdim];
    	yrange = Range[ydim];
    
    	rgbarray = Developer`ToPackedArray@Table[0, {xdim}, {ydim}];
    	If[cdif > 0.,
     	Scan[(col = #;
        	Scan[(
        	{r,g,b,a} = carray[[col,#]];
          	rval = Floor[ff * (r - min)/cdif];
           	gval = BitShiftLeft[Floor[ff * (g - min)/cdif], 8];
           	bval = BitShiftLeft[Floor[ff * (b - min)/cdif], 16];
           	aval = BitShiftLeft[Floor[ff * (a - min)/cdif], 24];
           	rgbarray[[col, #]] = aval+bval+gval+rval
           	) &, yrange]
        	) &
      	, xrange], (* else not rescale *)
     	Scan[(col = #;
        	Scan[(
        	{r,g,b,a} = carray[[col,#]];
           	rval = Floor[ff * r ];
           	gval = BitShiftLeft[Floor[ff * g], 8];           	
           	bval = BitShiftLeft[Floor[ff * b], 16];           	
           	aval = BitShiftLeft[Floor[ff * a], 24];
           	rgbarray[[col, #]] = aval + bval +gval + rval
           	) &, yrange]
        	) &
      	, xrange]
     	];
    	rgbarray
    ], CompilationTarget -> $cTarget]

ToUInteger32[im_Image] /; ImageChannels[im] === 1 := touint32[Reverse@ImageData[im], {0.,0.}, 255]
ToUInteger32[im_Image] /; ImageChannels[im] === 3 := to3uint32[Reverse@ImageData[im], {0.,0.}, 255]
ToUInteger32[im_Image] /; ImageChannels[im] === 4 := to4uint32[Reverse@ImageData[im], {0.,0.}, 255]
ToUInteger32[Graphics[Raster[data_, rest___], ___]] := to3uint32[data, {0.,0.}, 1]
*)

ToUInteger32[gr:(_Graphics|_Graphics3D), opts : OptionsPattern[]] := 
	Module[{rsize = OptionValue[RasterSize], r},
		r = Rasterize[gr, RasterSize->rsize];
		ToUInteger32[r]		
	]

(* ::Section:: *)
(* ExpressionModelClass *)

(*
	Base class for Expression models that represent a computation
    to be carried out on the client-side.
*)

ExpressionModelClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			}
	]
	
ExpressionModelClass.init[opts : OptionsPattern[ExpressionModelClass] ] :=
	Module[{attrs},
		o.type	= "Expression";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* StackModelClass *)

StackModelClass =
	NewClass[
		"Parents"	-> {ExpressionModelClass},
		"Fields"	-> {
			"fields"	-> {Default}	(* Seq(String, default=[], *)
(*
    A sequence of fields from a ``ColumnDataSource`` to sum (elementwise). For
    example:

    .. code-block:: python

        Stack(fields=['sales', 'marketing'])

    Will compute an array of values (in the browser) by adding the elements
    of the ``'sales'`` and ``'marketing'`` columns of a data source.
*)	
		}
	]
	
StackModelClass.init[opts : OptionsPattern[StackModelClass] ] :=
	Module[{attrs},
		o.type	= "Stack";
		
		If[o.fields === {Default}, o.fields = {}];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* SelectionModelClass *)

(*
    A Selection represents a portion of the data in a DataSource, which
    can be visually manipulated in a plot.

    Selections are typically created by selecting points in a plot with
    a SelectTool, but can also be programmatically specified.
*)

SelectionModelClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			"indices"				-> {} (* Seq(Int, default=[], *)
			, "line_indices"		-> {} (* Seq(Int, default=[], *)
			, "multiline_indices"	-> "Dict"[<||>] (* Dict(String, Seq(Int), default={} *)
			}
	]
	
SelectionModelClass.init[opts : OptionsPattern[SelectionModelClass] ] :=
	Module[{attrs},
		o.type	= "Selection";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* SelectionPolicyClass *)

(* abstract *)
SelectionPolicyClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			}
	]
	
SelectionPolicyClass.init[opts : OptionsPattern[SelectionPolicyClass] ] :=
	Module[{attrs},
		o.type	= "SelectionPolicy";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* IntersectRenderersClass *)

(*
    When a data source is shared between multiple renderers, a row in the data
    source will only be selected if that point for each renderer is selected. The
    selection is made from the intersection of hit test results from all renderers.
*)
IntersectRenderersClass =
	NewClass[
		"Parents"	-> {SelectionPolicyClass},
		"Fields"	-> {
			}
	]
	
IntersectRenderersClass.init[opts : OptionsPattern[IntersectRenderersClass] ] :=
	Module[{attrs},
		o.type	= "IntersectRenderers";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* UnionRenderersClass *)

(*
    When a data source is shared between multiple renderers, selecting a point on
    from any renderer will cause that row in the data source to be selected. The
    selection is made from the union of hit test results from all renderers.

*)
UnionRenderersClass =
	NewClass[
		"Parents"	-> {SelectionPolicyClass},
		"Fields"	-> {
			}
	]
	
UnionRenderersClass.init[opts : OptionsPattern[UnionRenderersClass] ] :=
	Module[{attrs},
		o.type	= "UnionRenderers";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* DataSourceClass *)

DataSourceClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
				"selected"		-> "Instance"[SelectionModelClass]
				, "callback" 	-> "Instance"[CallbackClass]
			}
	]
	
DataSourceClass.init[opts : OptionsPattern[DataSourceClass] ] :=
	Module[{attrs},
		o.type	= "DataSource";
		
		If[o.callback === CallbackClass, o.callback = Null];
		If[o.selected === SelectionModelClass, o.selected = New[SelectionModelClass][] ];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsection:: *)
(* ColumnarDataSourceClass *)

(* abstract *)
ColumnarDataSourceClass =
	NewClass[
		"Parents"	-> {DataSourceClass},
		"Fields"	-> {
			"selection_policy"	-> "Instance"[SelectionPolicyClass]
			}
	]
	
ColumnarDataSourceClass.init[opts : OptionsPattern[ColumnarDataSourceClass] ] :=
	Module[{attrs},
		o.type	= "ColumnarDataSource";
		
		If[o."selection_policy" === SelectionPolicyClass, 
			o."selection_policy" = UnionRenderers[]];

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* ColumnDataSource *)

ColumnDataSourceClass =
	NewClass[
		"Parents"	-> {ColumnarDataSourceClass},
		"Fields"	-> {
				"_args"		-> {"data"}
				, "data"	-> "ColumnData"[<||>]
			}
	]
	
ColumnDataSourceClass.init[opts : OptionsPattern[ColumnDataSourceClass] ] :=
	Module[{attrs, names},
		o.type	= "ColumnDataSource";

		If[!AssociationQ[o."data"],
			o."data" = <||>;
			Return[]
		];
(*
		names = GetSymbolName[#]& /@ Keys[o."data"];
		o."column_names" = names;
*)
	
		If[!FreeQ[Head /@ Keys[o."data"], Symbol],
			o."data" = Association[MapThread[Rule, {names, Values[o."data"]}]]
		];

		(* attribute fields *)
		attrs = o.attributes;
	]

arrayLength[a_Association]	/; KeyExistsQ[a, "shape"] := First[a["shape"]]
arrayLength[a_?VectorQ] := Length[a]
arrayLength[_] := -1	(* Should never happen *)
	
ColumnDataSourceClass.dimensions[] := {arrayLength[(o."data")[[1]]] , Length[o."data"]}
	
ColumnDataSourceClass.columnnames[] := Keys[o."data"]


Clear[encodeData]
encodeData[im_Image] := 
	Module[{array, encode64, dims},
		dims = Reverse@ImageDimensions[im];
		array = Flatten[ToUInteger32[im]];
		encode64 = ExportString[
						ExportString[array, "UnsignedInteger32"], 
							{"Base64", "String"}, CharacterEncoding -> "UTF8"
					];
		<|"__ndarray__"->encode64, "dtype"->"uint32", "shape"->dims|>
	]
encodeData[List[ im_ ]] := {encodeData[im]}

encodeData[r:Graphics[Raster[data_, ___], ___]] :=
	Module[{array, encode64, dims},
		dims = Part[Dimensions[data], {1,2}];
		array = Flatten[ToUInteger32[r]];
		encode64 = ExportString[
						ExportString[array, "UnsignedInteger32"], 
							{"Base64", "String"}, CharacterEncoding -> "UTF8"
					];
		<|"__ndarray__"->encode64, "dtype"->"uint32", "shape"->dims|>
	]
encodeData[List[ r:(Graphics[Raster[_, ___], ___]) ]] := {encodeData[r]}

encodeData[UInteger32[data_, rest___]] /; MatrixQ[data] && Length[Dimensions[data]] === 3 :=
	Module[{array, encode64, dims},
		dims = Part[Dimensions[data], {1,2}];
		array = Flatten[ToUInteger32[data, rest]];
		encode64 = ExportString[
						ExportString[array, "UnsignedInteger32"], 
							{"Base64", "String"}, CharacterEncoding -> "UTF8"
					];
		<|"__ndarray__"->encode64, "dtype"->"uint32", "shape"->dims|>
	]

(* floats *)
$floatEncodeLimit = 20
encodeData[data_] /; VectorQ[data] && MachineNumberQ[First[data]] && Length[data]>$floatEncodeLimit :=
	Module[{array, dims, encode64},
		array = toPackedArray[data];
		If[!packedArrayQ[array], Return[data]];
		dims = Dimensions[data];
		encode64 = StringReplace[
						ExportString[ExportString[array, "Real64"], {"Base64", "String"}, 
  						CharacterEncoding -> "UTF8"], 
  						"\n" -> ""];
  		<|"__ndarray__"->encode64, "dtype" -> "float64","shape"-> dims|>
	]

$intEncodeLimit = 20
encodeData[data_] /; VectorQ[data] && IntegerQ[First[data]] && Length[data] > $intEncodeLimit :=
	Module[{array, dims, encode64},
		array = toPackedArray[data];
		If[!packedArrayQ[array], Return[data]];
		dims = Dimensions[data];
		encode64 = StringReplace[
						ExportString[ExportString[array, "Integer32"], {"Base64", "String"}, 
  						CharacterEncoding -> "UTF8"], 
  						"\n" -> ""];
  		<|"__ndarray__"->encode64, "dtype" -> "int32","shape"-> dims|>
	]

encodeData[d_] := d

cdsadd[ColumnDataSourceClass[o_, ___], data_, name_, compress_] := 
	Module[{edata},
		edata = If[compress, encodeData[data], data];
		o["data"][name] = edata
	]


ColumnDataSourceClass.add[odata_, name_:None, compress_:True] :=
	Module[{sname = ToString[name], n},
		If[name === None,
			n = Length[o."data"];
			While[MemberQ[o."data", "Series "<>ToString[n]], n++];
			sname = "Series "<>ToString[n]
		];
		cdsadd[o, odata, sname, compress];
		sname
	]
	
cdsremove[ColumnDataSourceClass[o_, ___], name_] := 
	Module[{},
		If[!KeyExistsQ[o["data"], name], Return[]];
		o["data"] = KeyDrop[o["data"], name];
	]
ColumnDataSourceClass.remove[name_String] :=
	Module[{},
		cdsremove[o, name];
		None
	]

(*SetAttributes[decodeData, HoldFirst] *)
decodeData[a_Association] /; KeyExistsQ[a, "__ndarray__"] :=
	Switch[a["dtype"],
		"float64",
			ImportString[ImportString[a["__ndarray__"], {"Base64", "String"}], "Real64"],
		"int32",
			ImportString[ImportString[a["__ndarray__"], {"Base64", "String"}], "Integer32"],
		"uint32",
			ImportString[ImportString[a["__ndarray__"], {"Base64", "String"}], "UnsignedInteger32"],
		_, a["__ndarray__"]
	]
decodeData[a_] := a

ColumnDataSourceClass.getdata[index] := If[Length[(o."data")]>0, Range[Length[First[o."data"]]], {}]
ColumnDataSourceClass.getdata[name_String] := decodeData[(o."data")[name]]
ColumnDataSourceClass.getdata[lnames_List] := 
	If[And @@ StringQ /@ lnames, decodeData[(o."data")[#]]& /@ lnames, $Failed]
ColumnDataSourceClass.getdataTable[lnames_List] :=  
	If[And @@ StringQ /@ lnames, Transpose[o.getdata[lnames]], $Failed]

(* ::Subsubsection:: *)
(* RemoteSourceClass *)

RemoteSourceClass =
	NewClass[
		"Parents"	-> {ColumnDataSourceClass},
		"Fields"	-> {
			
			}
	]
	
RemoteSourceClass.init[opts : OptionsPattern[RemoteSourceClass] ] :=
	Module[{attrs},
		o.type	= "RemoteSource";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* AjaxDataSourceClass *)

AjaxDataSourceClass =
	NewClass[
		"Parents"	-> {RemoteSourceClass},
		"Fields"	-> {
			
			}
	]
	
AjaxDataSourceClass.init[opts : OptionsPattern[AjaxDataSourceClass] ] :=
	Module[{attrs},
		o.type	= "AjaxDataSource";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Subsubsection:: *)
(* GeoJSONDataSourceClass *)

GeoJSONDataSourceClass =
	NewClass[
		"Parents"	-> {ColumnarDataSourceClass},
		"Fields"	-> {
			
			}
	]
	
GeoJSONDataSourceClass.init[opts : OptionsPattern[GeoJSONDataSourceClass] ] :=
	Module[{attrs},
		o.type	= "GeoJSONDataSource";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* CDSViewClass *)

CDSViewClass =
	NewClass[
		"Parents"	-> {ModelClass},
		"Fields"	-> {
			"filters"		-> {}	(* List(Instance(Filter), default=[], *)			
			, "source"		-> "Instance"[ColumnarDataSourceClass]
			}
	]
	
CDSViewClass.init[opts : OptionsPattern[CDSViewClass] ] :=
	Module[{attrs},
		o.type	= "CDSView";

		(* attribute fields *)
		attrs = o.attributes;
	]
