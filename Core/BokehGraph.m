(* Wolfram Language package *)

(* Copyright 2020 by 1010data, Inc. Written by Ulises Cervantes-Pimentel *)

(* ::Section:: *)
(* BokehGraph *)

Clear[sowBokehNodes]
sowBokehNodes[b_BagClass, parent_] := Module[{},
  Scan[sowBokehNodes[#, parent] &,  (b.all[])]]

sowBokehNodes[b_Association, parent_] := Module[{},
  Scan[sowBokehNodes[#, parent] &,  Values[b] ]]

sowBokehNodes[b_List, parent_] := Module[{},
  Scan[sowBokehNodes[#, parent] &,  b]]

sowBokehNodes[p : MBokehClassPatterns, parent_: Null] :=
	Module[{fields},
		If[p.$flagged === True, 
			Sow[(parent.attributeID[])["id"] -> (p.attributeID[])["id"]]
		];
		If[MissingQ[p.$flagged] || ! (p.$flagged),
			p.updateAttributes[reset];
			fields = Values@Part[p.attributes, 1];
			(p.$flagged) = True;
			Scan[sowBokehNodes[#, p] &, fields];
			If[parent =!= Null,
				Sow[(parent.attributeID[])["id"] -> (p.attributeID[])["id"]]
			]
		];
  ]
sowBokehNodes[p_, ___] := Null

collectBokehNodes[p : MBokehClassPatterns] :=
	Module[{},
		ResetBokehInstancesFlag[];
		Flatten@Union[Last@Reap[sowBokehNodes[p]], 
			SameTest -> ((#1).attributeID[] == (#2).attributeID[] &)]
  ]

Options[BokehGraph] = Options[GraphPlot]
BokehGraph[p : MBokehClassPatterns, opts : OptionsPattern[] ] :=
	Module[{s1, assoc},
		assoc = Association[(#.attributeID[])["id"] -> #.attributeID[] & /@ \
					($MasterBokehBag.all[]) ];
		s1 = collectBokehNodes[p];
		
		GraphPlot[s1, opts, 
			VertexLabeling	-> Tooltip, 
			ImageSize		-> 800,
			MultiedgeStyle	-> True, 
			DirectedEdges	-> True,
			VertexRenderingFunction -> 
				({White, EdgeForm[Black], Disk[#, .2], Black, 
					Tooltip[Text[Style[#2, 9], #1], assoc[#2]["type"]]} &)]
  ]
