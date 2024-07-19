(* Wolfram Language package *)

(* ::Section:: *)
(* Tile Sources *)

(*
			, TileSourceClass
				, MercatorTileSourceClass
					, TMSTileSourceClass
					, WMTSTileSourceClass
					, QUADKEYTileSourceClass
					, BBoxTileSourceClass
*)


(* ::Section:: *)
(* TileSourceClass *)

TileSourceClass =
	NewClass[
		"Parents"		-> {ModelClass},
		"Fields"		-> {
			"_args"					-> {"url", "tile_size", "min_zoom", "max_zoom", "extra_url_vars"}
			, "url"					-> "BString"[""]
			, "tile_size"			-> "BInt"[256]
			, "min_zoom"			-> "BInt"[0]
			, "max_zoom"			-> "BInt"[30]
			, "extra_url_vars"		-> "Dict"[<||>]
			, "attribution"			-> "BString"[""]
			, "x_origin_offset"		-> "BFloat"[0]
			, "y_origin_offset"		-> "BFloat"[0]
			, "initial_resolution"	-> "BFloat"[0]
		}
	]
	
TileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "TileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* MercatorTileSourceClass *)

MercatorTileSourceClass =
	NewClass[
		"Parents"		-> {TileSourceClass},
		"Fields"		-> {
			"_args"					-> {"url", "tile_size", "min_zoom", "max_zoom", "x_origin_offset", "y_origin_offset", "extra_url_vars", "initial_resolution"}
			, "x_origin_offset"		-> "BFloat"[20037508.34]
			, "y_origin_offset"		-> "BFloat"[20037508.34]
			, "initial_resolution"	-> "BFloat"[156543.03392804097]
			, "snap_to_zoom"		-> "BBool"[False]
			, "wrap_around"			-> "BBool"[True]
(*
    ..note::
        Axis coordinates are not wrapped. To toggle axis label visibility,
        use ``plot.axis.visible = False``.
*)	
		}
	]
	
MercatorTileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "MercatorTileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* TMSTileSourceClass *)

TMSTileSourceClass =
	NewClass[
		"Parents"		-> {MercatorTileSourceClass},
		"Fields"		-> {
		}
	]
	
TMSTileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "TMSTileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* WMTSTileSourceClass *)

WMTSTileSourceClass =
	NewClass[
		"Parents"		-> {MercatorTileSourceClass},
		"Fields"		-> {
		}
	]
	
WMTSTileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "WMTSTileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]

(* ::Section:: *)
(* QUADKEYTileSourceClass *)

QUADKEYTileSourceClass =
	NewClass[
		"Parents"		-> {MercatorTileSourceClass},
		"Fields"		-> {
		}
	]
	
QUADKEYTileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "QUADKEYTileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]


(* ::Section:: *)
(* BBoxTileSourceClass *)

BBoxTileSourceClass =
	NewClass[
		"Parents"		-> {MercatorTileSourceClass},
		"Fields"		-> {
			"use_latlon"	-> "BBool"[False]
(*
    Flag which indicates option to output {XMIN},{YMIN},{XMAX},{YMAX} in meters or latitude and longitude.
*)		}
	]
	
BBoxTileSourceClass.init[opts_] :=
	Module[{attrs},
		o.type = "BBoxTileSource";

		(* attribute fields *)
		attrs = o.attributes;
	]

$CARTOATTRIBUTION = 
"&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors,
&copy; <a href=\"https://cartodb.com/attributions\">CartoDB</a>"

$STAMENATTRIBUTION = 
"Map tiles by <a href=\"https://stamen.com\">Stamen Design</a>,
under <a href=\"https://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>.
Data by <a href=\"https://openstreetmap.org\">OpenStreetMap</a>,
under `1`."

TileProviders["CARTODBPOSITRON"] := 
	WMTSTileSource[
		"url" -> "https://tiles.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
		, "attribution"	-> $CARTOATTRIBUTION
	]

TileProviders["CARTODBPOSITRON_RETINA"] := 
	WMTSTileSource[
		"url" -> "https://tiles.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png"
		, "attribution"	-> $CARTOATTRIBUTION
	]

TileProviders["STAMEN_TERRAIN"] := 
	WMTSTileSource[
		"url" -> "http://tile.stamen.com/terrain/{Z}/{X}/{Y}.png"
		, "attribution"	-> 
			StringTemplate[$STAMENATTRIBUTION][
				"<a href=\"https://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>"
				]
	]

TileProviders["STAMEN_TERRAIN_RETINA"] := 
	WMTSTileSource[
		"url" -> "http://tile.stamen.com/terrain/{Z}/{X}/{Y}@2x.png"
		, "attribution"	-> 
			StringTemplate[$STAMENATTRIBUTION][
				"<a href=\"https://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>"
				]
	]

TileProviders["STAMEN_TONER"] := 
	WMTSTileSource[
		"url" -> "http://tile.stamen.com/toner/{Z}/{X}/{Y}.png"
		, "attribution"	-> 
			StringTemplate[$STAMENATTRIBUTION][
				"<a href=\"https://www.openstreetmap.org/copyright\">ODbL</a>"
				]
	]

TileProviders["STAMEN_TONER_BACKGROUND"] := 
	WMTSTileSource[
		"url" -> "http://tile.stamen.com/toner-background/{Z}/{X}/{Y}.png"
		, "attribution"	-> 
			StringTemplate[$STAMENATTRIBUTION][
				"<a href=\"https://www.openstreetmap.org/copyright\">ODbL</a>"
				]
	]

TileProviders["STAMEN_TONER_LABELS"] := 
	WMTSTileSource[
		"url" -> "http://tile.stamen.com/toner-labels/{Z}/{X}/{Y}.png"
		, "attribution"	-> 
			StringTemplate[$STAMENATTRIBUTION][
				"<a href=\"https://www.openstreetmap.org/copyright\">ODbL</a>"
				]
	]
