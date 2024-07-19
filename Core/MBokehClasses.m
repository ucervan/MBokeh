(* Wolfram Language package *)

(* Copyright 2016-2021 by 1010data, Inc. Written by Ulises Cervantes-Pimentel *)

(* WARNING:
	Loading the paclage multiple times disables the method functions,
	GetFunctions[classname][classname] will return {} if called a second time.	
	Needs further review...
*)

Global`PackageTimingStart[]

BeginPackage["MBokeh`", {
	"MClasses`", 
	"MClasses`Core`MPlusPlus`",
	"TenTenFormat`"}]

(* Exported symbols added here with SymbolName::usage *)

(* ::Section:: *)
(* Unprotect and Clear symbols *)

$MBokehClassesPublic = {
	  MBokeh`OutputFile
	, MBokeh`index
	, MBokeh`value
	, MBokeh`field
	, MBokeh`expr
	, MBokeh`stack
	, MBokeh`Figure
	, MBokeh`figure
	, MBokeh`TileProviders
	, MBokeh`FactorCMap
	, MBokeh`IsInstanceQ
	, MBokeh`InEnumerationQ
	, MBokeh`HasAttrQ
	, MBokeh`ToUInteger32
	, MBokeh`UInteger32
	, MBokeh`BokehColorQ
	, MBokeh`row
	, MBokeh`column
	, MBokeh`widgetbox
	, MBokeh`layout
	, MBokeh`gridplot
	, MBokeh`gmap	
	, MBokeh`MBokehDisplayFunction
	, MBokeh`MBokehUnlock
	, MBokeh`MBokehClearAll
	, MBokeh`BokehGraph
}

(* ::Section:: *)
(* Public function/ symbols usage declaration*)

events::usage			= "events"
OutputFile::usage		= "OutputFile[filename]."
index::usage			= "index"
value::usage			= "value[attr]"
field::usage			= "field[attr]"
expr::usage				= "expr[attr]"
stack::usage			= "stack[fields]"
Figure::usage			= "Figure[ opts ... ]."
figure::usage			= "figure[ opts ... ]."
TileProviders::usage	= "TileProviders[]"
FactorCMap::usage		= "FactorCMap"
IsInstanceQ::usage		= "IsInstanceQ[p,class]."
InEnumerationQ::usage	= "InEnumerationQ[v, enum]"
HasAttrQ::usage			= "HasAttrQ[p,attr]"
ToUInteger32::usage		= "ToUInteger32[image]"
UInteger32::usage		= "UInteger32[array]"
BokehColorQ::usage		= "BokehColorQ[]"
row::usage				= "row[]"
column::usage			= "column[]"
widgetbox::usage		= "widgetbox[children___, opts]"
layout::usage			= "layout[]"
gridplot::usage			= "gridplot[elems]"
gmap::usage				= "gmap[]"
MBokehUnlock::usage		= "MBokehUnlockInstances[p]"
MBokehClearAll::uage	= "MBokehClearAll[], reset Bokeh objects bag."

$MBokehMethods = {
	  MBokeh`callback
	, MBokeh`select

	, MBokeh`Event
	, MBokeh`ButtonClick
	, MBokeh`LODStart
	, MBokeh`LODEnd
	, MBokeh`SelectionGeometry
	, MBokeh`Reset
	, MBokeh`TapEvent
	, MBokeh`DoubleTap
	, MBokeh`Press
	, MBokeh`MouseEnter
	, MBokeh`MouseLeave
	, MBokeh`MouseMove
	, MBokeh`MouseWheel
	, MBokeh`Pan
	, MBokeh`PanEnd
	, MBokeh`PanStart
	, MBokeh`Pinch
	, MBokeh`PinchEnd
	, MBokeh`PinchStart
}

(Unprotect[#]; ClearAll[#])& /@ $MBokehClassesPublic
(Unprotect[#]; ClearAll[#])& /@ $MBokehMethods

MBokehUtilityClasses = {
	BagClass
	, ListAttrSplatClass
	, EventConstructorClass
	, ThemeClass
}

MBokehClasses = {
	BokehAttributeClass
		, ModelClass
			, TileSourceClass
				, MercatorTileSourceClass
					, TMSTileSourceClass
					, WMTSTileSourceClass
					, QUADKEYTileSourceClass
					, BBoxTileSourceClass
			, TransformClass
				, CustomJSTransformClass
				, DodgeClass
				, JitterClass
				, InterpolatorClass
					, LinearInterpolatorClass
					, StepInterpolatorClass
				, ColorMapperClass
					, CategoricalColorMapperClass
					, ContinousColorMapperClass
						, LinearColorMapperClass
						, LogColorMapperClass
			, LayoutDOMClass
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
			
				, ToolbarBoxClass
				, SpacerBoxClass
				, WidgetBoxClass
				, BoxModelClass
					, RowBoxModelClass
					, ColumnBoxClass
			, MapOptionsClass
				, GMapOptionsClass
			, CallbackClass
				, OpenURLClass
				, CustomJSClass
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
			, GlyphClass
				, XYGlyphClass
					, AnnularWedgeClass
					, GlyphAnnulusClass
					, ArcClass
					, EllipseClass
					, GlyphImageClass
					, ImageRGBAClass
					, ImageURLClass
					, GlyphLineClass
					, OvalClass
					, PatchClass
					, RayClass
					, RectClass
					, StepClass
					, GlyphTextClass
					, GlyphWedgeClass
					, MarkerClass
						, MarkerAsteriskClass
						, MarkerCircleClass
						, MarkerCircleCrossClass
						, MarkerCircleXClass
						, MarkerCrossClass
						, MarkerDiamondClass
						, MarkerDiamondCrossClass
						, MarkerHexClass
						, MarkerInvertedTriangleClass
						, MarkerSquareClass
						, MarkerSquareCrossClass
						, MarkerSquareXClass
						, MarkerTriangleClass
						, MarkerXClass
				, BezierClass
				, HBarClass
				, HexTileClass
				, MultiLineClass
				, PatchesClass
				, QuadClass
				, QuadricClass
				, SegmentClass
				, VBarClass
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
			, TickFormatterClass
				, BasicTickFormatterClass
					, MercatorTickFormatterClass
				, NumeralTickFormatterClass
				, PrintfTickFormatterClass
				, LogTickFormatterClass
				, CategoricalTickFormatterClass
				, FuncTickFormatterClass
				, DatetimeTickFormatterClass
			, RendererClass
				, DataRendererClass
					, TileRendererClass
					, DynamicImageRendererClass
					, GlyphRendererClass
					, GraphRendererClass
				, GuideRendererClass
					, GridModelClass
					, AxisModelClass
						, ContinousAxisClass
							, LinearAxisClass
								, DatetimeAxisClass
								, MercatorAxisClass
							, LogAxisClass
						, CategoricalAxisClass
				, AnnotationModelClass
					, TextAnnotationClass
					, LegendClass
					, TitleClass
					, TooltipModelClass
					, BoxAnnotationClass
					, ColorBarClass
					, LabelModelClass
					, PolyAnnotationClass
			, LegendItemClass
			, ToolbarBaseClass
				, ToolbarClass
				, ProxyToolbarClass
			, ToolClass
				, ActionClass
				, DragClass
				, ScrollClass
				, TapClass
				, InspectionClass
			, PanToolClass
			, WheelPanToolClass
			, WheelZoomToolClass
			, SaveToolClass
			, ResetToolClass
			, TapToolClass
			, CrosshairToolClass
			, BoxZoomToolClass
			, ZoomInToolClass
			, ZoomOutToolClass
			, BoxSelectToolClass
			, LassoSelectToolClass
			, PolySelectToolClass
			, CustomJSHoverClass
			, HoverToolClass
			, HelpToolClass
			, UndoToolClass
			, RedoToolClass
			, EditToolClass
				, BoxEditToolClass
				, PointDrawToolClass
				, PolyDrawToolClass
				, PolyEditToolClass
		, RangeClass
			, Range1dClass
			, DataRangeClass
				, DataRange1dClass
			, FactorRangeClass
		, ScaleClass
			, LinearScaleClass
				, CategoricalScaleClass
			, LogScaleClass
		, ColumnDataSourceClass
		, FigureOptionsClass
		, GMapFigureOptionsClass
		, PlotModelClass
			, FigureClass
			, MapPlotClass
			, GMapPlotClass
				, GMapClass
}

MBokehEventClasses = {
	EventClass
		, ButtonClickClass
		, LODStartClass
		, LODEndClass
		, SelectionGeometryClass
		, ResetClass
		, TapEventClass
		, DoubleTapClass		
		, PressClass
		, MouseEnterClass
		, MouseLeaveClass
		, MouseMoveClass
		, MouseWheelClass
		, PanClass
		, PanEndClass
		, PanStartClass
		, PinchClass
		, PinchEndClass
		, PinchStartClass		
}

(* ::Section:: *)
(* Classes usage declaration *)

EventConstructorClass::usage		= "EventConstructorClass"
EventClass::usage					= "EventClass"
ButtonClickClass::usage				= "ButtonClickClass"
LODStartClass::usage				= "LODStartClass"
LODEndClass::usage					= "LODEndClass"
SelectionGeometryClass::usage		= "SelectionGeometryClass"
ResetClass::usage					= "ResetClass"
TapEventClass::usage				= "TapEventClass" 
DoubleTapClass::usage				= "DoubleTapClass"
PressClass::usage					= "PressClass"
MouseEnterClass::usage				= "MouseEnterClass"
MouseLeaveClass::usage				= "MouseLeaveClass"
MouseMoveClass::usage				= "MouseMoveClass"
MouseWheelClass::usage				= "MouseWheelClass"
PanClass::usage						= "PanClass"
PanEndClass::usage					= "PanEndClass"
PanStartClass::usage				= "PanStartClass"
PinchClass::usage					= "PinchClass"
PinchEndClass::usage				= "PinchEndClass"
PinchStartClass::usage				= "PinchStartClass"

BagClass::usage						= "BagClass wrapper for Internal`Bag"
ListAttrSplatClass::usage			= "ListAttrSplatClass"
ThemeClass::usage					= "ThemeClass"

BokehAttributeClass::usage			= "BokehAttributeClass"
ModelClass::usage					= "ModelClass"

TileSourceClass::usage				= "TileSourceClass"
MercatorTileSourceClass::usage		= "MercatorTileSourceClass"
TMSTileSourceClass::usage			= "TMSTileSourceClass"
WMTSTileSourceClass::usage			= "WMTSTileSourceClass"
QUADKEYTileSourceClass::usage		= "QUADKEYTileSourceClass"
BBoxTileSourceClass::usage			= "BBoxTileSourceClass"

TransformClass::usage				= "TransformClass"
CustomJSTransformClass::usage		= "CustomJSTransformClass"
DodgeClass::usage					= "DodgeClass"
JitterClass::usage					= "JitterClass"
InterpolatorClass::usage			= "InterpolatorClass"
LinearInterpolatorClass::usage		= "LinearInterpolatorClass"
StepInterpolatorClass::usage		= "StepInterpolatorClass"
ColorMapperClass::usage				= "ColorMapperClass"
CategoricalColorMapperClass::usage	= "CategoricalColorMapperClass"
ContinousColorMapperClass::usage	= "ContinousColorMapperClass"
LinearColorMapperClass::usage		= "LinearColorMapperClass"
LogColorMapperClass::usage			= "LogColorMapperClass"

LayoutDOMClass::usage				= "LayoutDOMClass"
ToolbarBoxClass::usage				= "ToolbarBoxClass"
SpacerBoxClass::usage				= "SpacerBoxClass"

WidgetClass::usage					= "WidgetClass"
PanelWidgetClass::usage				= "PanelWidgetClass"
TabsClass::usage					= "TabsClass"
MarkupClass::usage					= "MarkupClass"
ParagraphClass::usage				= "ParagraphClass"
DivWidgetClass::usage				= "DivWidgetClass"
PreTextClass::usage					= "PreTextClass"
AbstractIconClass::usage			= "AbstractIconClass"
AbstractGroupClass::usage			= "AbstractGroupClass"
ButtonGroupClass::usage				= "ButtonGroupClass"
GroupClass::usage					= "GroupClass"
CheckboxGroupClass::usage			= "CheckboxGroupClass"
RadioGroupClass::usage				= "RadioGroupClass"
CheckboxButtonGroupClass::usage		= "CheckboxButtonGroupClass"
RadioButtonGroupClass::usage		= "RadioButtonGroupClass"
InputWidgetClass::usage				= "InputWidgetClass"
TextInputClass::usage				= "TextInputClass"
PasswordInputClass::usage			= "PasswordInputClass"
AutocompleteInputClass::usage		= "AutocompleteInputClass"
SelectWidgetClass::usage			= "SelectWidgetClass"
MultiSelectClass::usage				= "MultiSelectClass"
DatePickerClass::usage				= "DatePickerClass"
ButtonLikeClass::usage				= "ButtonLikeClass"
AbstractButtonClass::usage			= "AbstractButtonClass"
ButtonWidgetClass::usage			= "ButtonWidgetClass"
ToggleWidgetClass::usage			= "ToggleWidgetClass"
DropdownWidgetClass::usage			= "DropdownWidgetClass"
AbstractSliderClass::usage			= "AbstractSliderClass"
SliderWidgetClass::usage			= "SliderWidgetClass"
RangeSliderClass::usage				= "RangeSliderClass"
DateSliderClass::usage				= "DateSliderClass"
DateRangeSliderClass::usage			= "DateRangeSliderClass"

WidgetBoxClass::usage				= "WidgetBoxClass"
BoxModelClass::usage				= "BoxModelClass"
RowBoxModelClass::usage				= "RowBoxModelClass"
ColumnBoxClass::usage				= "ColumnBoxClass"

MapOptionsClass::usage				= "MapOptionsClass"
GMapOptionsClass::usage				= "GMapOptionsClass"

CallbackClass::usage				= "CallbackClass"
OpenURLClass::usage					= "OpenURLClass"
CustomJSClass::usage				= "CustomJSClass"

SelectionModelClass::usage			= "SelectionModelClass"
SelectionPolicyClass::usage			= "SelectionPolicyClass"
IntersectRenderersClass::usage		= "IntersectRenderersClass"
UnionRenderersClass::usage			= "UnionRenderersClass"

ExpressionModelClass::usage			= "ExpressionModelClass"
StackModelClass::usage				= "StackModelClass"
DataSourceClass::usage				= "DataSourceClass"
ColumnarDataSourceClass::usage		= "ColumnarDataSourceClass"
ColumnDataSourceClass::usage		= "ColumnDataSourceClass"
RemoteSourceClass::usage			= "RemoteSourceClass"
AjaxDataSourceClass::usage			= "AjaxDataSourceClass"
GeoJSONDataSourceClass::usage		= "GeoJSONDataSourceClass"
CDSViewClass::usage					= "CDSViewClass"

GlyphClass::usage					= "GlyphClass"
XYGlyphClass::usage					= "XYGlyphClass"
AnnularWedgeClass::usage			= "AnnularWedgeClass"
GlyphAnnulusClass::usage			= "GlyphAnnulusClass"
ArcClass::usage						= "ArcClass"
EllipseClass::usage					= "EllipseClass"
GlyphImageClass::usage				= "GlyphImageClass"
ImageRGBAClass::usage				= "ImageRGBAClass"
ImageURLClass::usage				= "ImageURLClass"
GlyphLineClass::usage				= "GlyphLineClass"
OvalClass::usage					= "OvalClass"
PatchClass::usage					= "PatchClass"
RayClass::usage						= "RayClass"
RectClass::usage					= "RectClass"
StepClass::usage					= "StepClass"
GlyphTextClass::usage				= "GlyphTextClass"
GlyphWedgeClass::usage				= "GlyphWedgeClass"

MarkerClass::usage					= "MarkerClass"
MarkerAsteriskClass::usage			= "MarkerAsteriskClass"
MarkerCircleClass::usage			= "MarkerCircleClass"
MarkerCircleCrossClass::usage		= "MarkerCircleCrossClass"
MarkerCircleXClass::usage			= "MarkerCircleXClass"
MarkerCrossClass::usage				= "MarkerCrossClass"
MarkerDiamondClass::usage			= "MarkerDiamondClass"
MarkerDiamondCrossClass::usage		= "MarkerDiamondCrossClass"
MarkerHexClass::usage				= "MarkerHexClass"
MarkerInvertedTriangleClass::usage	= "MarkerInvertedTriangleClass"
MarkerSquareClass::usage			= "MarkerSquareClass"
MarkerSquareCrossClass::usage		= "MarkerSquareCrossClass"
MarkerSquareXClass::usage			= "MarkerSquareXClass"
MarkerTriangleClass::usage			= "MarkerTriangleClass"
MarkerXClass::usage					= "MarkerXClass"

BezierClass::usage					= "BezierClass"
HBarClass::usage					= "HBarClass"
HexTileClass::usage					= "HexTileClass"
MultiLineClass::usage				= "MultiLineClass"
PatchesClass::usage					= "PatchesClass"
QuadClass::usage					= "QuadClass"
QuadricClass::usage					= "QuadricClass"
SegmentClass::usage					= "SegmentClass"
VBarClass::usage					= "VBarClass"

TickerClass::usage					= "TickerClass"
ContinuousTickerClass::usage		= "ContinuousTickerClass"
FixedTickerClass::usage				= "FixedTickerClass"
AdaptiveTickerClass::usage			= "AdaptiveTickerClass"
BasicTickerClass::usage				= "BasicTickerClass"
MercatorTickerClass::usage			= "MercatorTickerClass"
LogTickerClass::usage				= "LogTickerClass"
CompositeTickerClass::usage			= "CompositeTickerClass"
DatetimeTickerClass::usage			= "DatetimeTickerClass"
SingleIntervalTickerClass::usage	= "SingleIntervalTickerClass"
DaysTickerClass::usage				= "DaysTickerClass"
MonthsTickerClass::usage			= "MonthsTickerClass"
YearsTickerClass::usage				= "YearsTickerClass"
CategoricalTickerClass::usage		= "CategoricalTickerClass"

TickFormatterClass::usage			= "TickFormatterClass"
BasicTickFormatterClass::usage		= "BasicTickFormatterClass"
MercatorTickFormatterClass::usage	= "MercatorTickFormatterClass"
NumeralTickFormatterClass::usage	= "NumeralTickFormatterClass"
PrintfTickFormatterClass::usage		= "PrintfTickFormatterClass"
LogTickFormatterClass::usage		= "LogTickFormatterClass"
CategoricalTickFormatterClass::usage= "CategoricalTickFormatterClass"
FuncTickFormatterClass::usage		= "FuncTickFormatterClass"
DatetimeTickFormatterClass::usage	= "DatetimeTickFormatterClass"

ToolbarBaseClass::usage				= "ToolbarBaseClass"
ToolbarClass::usage					= "ToolbarClass"
ProxyToolbarClass::usage			= "ProxyToolbarClass"
ToolClass::usage					= "ToolClass"
ActionClass::usage					= "ActionClass"
DragClass::usage					= "DragClass"
ScrollClass::usage					= "ScrollClass"
TapClass::usage						= "TapClass"
InspectionClass::usage				= "InspectionClass"
PanToolClass::usage					= "PanToolClass"
WheelPanToolClass::usage			= "WheelPanToolClass"
WheelZoomToolClass::usage			= "WheelZoomToolClass"
SaveToolClass::usage				= "SaveToolClass"
ResetToolClass::usage				= "ResetToolClass"
TapToolClass::usage					= "TapToolClass"
CrosshairToolClass::usage			= "CrosshairToolClass"
BoxZoomToolClass::usage				= "BoxZoomToolClass"
ZoomInToolClass::usage				= "ZoomInToolClass"
ZoomOutToolClass::usage				= "ZoomOutToolClass"
BoxSelectToolClass::usage			= "BoxSelectToolClass"
LassoSelectToolClass::usage			= "LassoSelectToolClass"
PolySelectToolClass::usage			= "PolySelectToolClass"
CustomJSHoverClass::usage			= "CustomJSHoverClass"
HoverToolClass::usage				= "HoverToolClass"
HelpToolClass::usage				= "HelpToolClass"
UndoToolClass::usage				= "UndoToolClass"
RedoToolClass::usage				= "RedoToolClass"
EditToolClass::usage				= "EditToolClass"
BoxEditToolClass::usage				= "BoxEditToolClass"
PointDrawToolClass::usage			= "PointDrawToolClass"
PolyDrawToolClass::usage			= "PolyDrawToolClass"
PolyEditToolClass::usage			= "PolyEditToolClass"

PlotModelClass::usage				= "PlotModelClass"
RangeClass::usage					= "RangeClass"
Range1dClass::usage					= "Range1dClass"
DataRangeClass::usage				= "DataRangeClass"
DataRange1dClass::usage				= "DataRange1dClass"
ScaleClass::usage					= "ScaleClass"
LinearScaleClass::usage				= "LinearScaleClass"
CategoricalScaleClass::usage		= "CategoricalScaleClass"
LogScaleClass::usage				= "LogScaleClass"
FactorRangeClass::usage				= "FactorRangeClass"
ColumnDataSourceClass::usage		= "ColumnDataSourceClass"
FigureClass::usage					= "FigureClass"
MapPlotClass::usage					= "MapPlotClass"
GMapPlotClass::usage				= "GMapPlotClass"
GMapFigureOptionsClass::usage		= "GMapFigureOptionsClass"
GMapClass::usage					= "GMapClass"
FigureOptionsClass::usage			= "FigureOptionsClass"

RendererClass::usage				= "RendererClass"
DataRendererClass::usage			= "DataRendererClass"
TileRendererClass::usage			= "TileRendererClass"
DynamicImageRendererClass::usage	= "DynamicImageRendererClass"
GlyphRendererClass::usage			= "GlyphRendererClass"
GraphRendererClass::usage			= "GraphRendererClass"
GuideRendererClass::usage			= "GuideRendererClass"
AxisModelClass::usage				= "AxisModelClass"
ContinousAxisClass::usage			= "ContinousAxisClass"
LinearAxisClass::usage				= "LinearAxisClass"
MercatorAxisClass::usage			= "MercatorAxisClass"
LogAxisClass::usage					= "LogAxisClass"
CategoricalAxisClass::usage			= "CategoricalAxisClass"
DatetimeAxisClass::usage			= "DatetimeAxisClass"
GridModelClass::usage				= "GridModelClass"
AnnotationModelClass::usage			= "AnnotationModelClass"
TextAnnotationClass::usage			= "TextAnnotationClass"
LegendItemClass::usage				= "LegendItemClass"
LegendClass::usage					= "LegendClass"
TitleClass::usage					= "TitleClass"
TooltipModelClass::usage			= "TooltipModelClass"
BoxAnnotationClass::usage			= "BoxAnnotationClass"
ColorBarClass::usage				= "ColorBarClass"
LabelModelClass::usage				= "LabelModelClass"
PolyAnnotationClass::usage			= "PolyAnnotationClass"


MBokehClassPatterns =  Alternatives @@ (Blank /@ Sort[MBokehClasses])
MBokehClassSymbols  =  Alternatives@@ Sort[MBokehClasses]

MBokehEventClassPatterns =  Alternatives @@ (Blank /@ Sort[MBokehEventClasses])
MBokehEventClassSymbols  =  Alternatives@@ Sort[MBokehEventClasses]

(ClearAll[#])& /@ MBokehClasses

(* ::Section:: *)
(* 	In the Public COntext section, create Wrapper functions. 
	Converts from options to arguments functions 
*)

(*
$allClasses = Join[Complement[MBokehClasses,{}], MBokehUtilityClasses, MBokehEventClasses] 
*)
$allClasses = Join[Complement[MBokehClasses,{}], MBokehUtilityClasses]

$safeSymbols = $GetSafeClasses[$allClasses]
$unsafeSymbols = $GetUnSafeClasses[$allClasses]
$RegisterClasses[$safeSymbols]

(* ::Section:: *)
(* Private Context *)

Begin["`Private`"]

(* Implementation of the package *)

GetSymbolName 	= MClasses`Utils`Utils`GetSymbolName
packedArrayQ 	= Developer`PackedArrayQ
toPackedArray 	= Developer`ToPackedArray

(* ::Section:: *)
(* Enums *)

Get["MBokeh`Core`Enums`"]


(* ::Section:: *)
(* Properties *)

Get["MBokeh`Core`Properties`"]

(* ::Section:: *)
(* Globals *)

(* ::Section:: *)
(* BokehGetID *)

BIDCounter	= 1;

(*
BIDType 	= "UID";
BIDType 	= {"EID"};
*)

BIDType :=  BIDType = 
	If[NameQ["$CloudEvaluation"] && ToExpression["$CloudEvaluation"],
		"UID",
		(* else *)
		{"EID"}
	]

Clear[BokehGetID]
BokehGetID["INC"] 			:= ToString[BIDCounter++]
BokehGetID["UID"] 			:= CreateUUID[]
BokehGetID[eid_String] 		:= CreateUUID[eid <> "-"]
BokehGetID[{eid_String}] 	:= eid <> "-" <> ToString[BIDCounter++]
BokehGetID[] 				:= BokehGetID[BIDType]

(* ::Section:: *)
(* BagClass *)

BagClass =
	NewClass[
		"Parents" 	-> {BaseClass},
		"Fields"	-> {"content" -> {}}		
	]
BagClass.init[opts : OptionsPattern[BagClass]] :=
	Module[{},
		o."bag" = Internal`Bag[];
		o.stuff[o.optionValue[opts,"content"]];
	]
dropBag[h_[o_,___]] := KeyDrop[o,"bag"]
BagClass.reset[] := (dropBag[o]; o."bag" = Internal`Bag[])
BagClass.optionValue[opts_, name_] := 
	OptionValue[Head[o], opts, name]
BagClass.len[] := 
	Length[Internal`BagPart[o."bag",All]]
	
isValidClassQ[p:h_[_Symbol, ___]] := (ListQ[Supers[h]] && ContainsAny[Supers[h], {BokehAttributeClass}])
isValidClassQ[p_]:=False
(* stuff *)
BagClass.stuff[p: MBokehClassPatterns] := 
	(Internal`StuffBag[o."bag", p]; o)
BagClass.stuff[p_?isValidClassQ]:=
	(Internal`StuffBag[o."bag", p]; o)
BagClass.stuff[p_, False]:=
	(Internal`StuffBag[o."bag", p]; o)
	
BagClass.stuff[{}] := 
	Null 
BagClass.stuff[l_List] := 
	(o.stuff[#]& /@ l; o)
BagClass.stuff[b_BagClass] :=
	(o.stuff[b.all[]])
BagClass.stuff[x_] := 
	(Print[{"Invalid object to stuff",x}]; Null)

(* part *)
BagClass.all[] := 
	Internal`BagPart[o."bag", All]
BagClass.part[i: (_Integer | All)] := 
	Internal`BagPart[o."bag", i]
BagClass.part[___] := 
	Null

(* json *)
BagClass.normalize[] :=
	(#.attributeID[])& /@ Internal`BagPart[o."bag",All]
BagClass.rawjson[depth_:4] := 
	ExportString[o.normalize[], "RawJSON", "Compact" -> depth]


(* ::Section:: *)
(* ListAttrSplatClass *)

ListAttrSplatClass = 
	NewClass[
		"Parents"	-> {BaseClass},
		"Fields"	-> {
			"_args"		-> {"data"}
			, "data"	-> {}
		}
	]
	
ListAttrSplatClass.len[] := Length[o."data"]
ListAttrSplatClass.part[i_]:= Module[{len=o.len[]}, Which[i<=0 || i>len, Null, True, Part[o."data",i]]]
ListAttrSplatClass.set[key_, value_] :=
	Module[{skey = GetSymbolName@key},
		Scan[(#.skey = value)&, o."data"];
	]

MClasses`Core`MPlusPlus`Private`fastOperation[p_ListAttrSplatClass, key_, value_, Set] := p.set[key, value]
DownValues[MClasses`Core`MPlusPlus`Private`fastOperation] = 
  RotateRight[
   DownValues[MClasses`Core`MPlusPlus`Private`fastOperation]];

ListAttrSplatClass /: Plus[ p1_ListAttrSplatClass, p2_ListAttrSplatClass] := ListAttrSplat[Join[p1."data", p2."data"]]


(* ::Section:: *)
(* ListAttrSplatClass *)

ThemeClass = 
	NewClass[
		"Parents"	-> {BaseClass},
		"Fields"	-> {
			"json"	-> {}
		}
	]
	
defaultModelTheme[p_] :=
	Module[{},
	(* canvas BG *)
	p."background_fill_color" = "#ffffff00";
	p."border_fill_color" = "#ffffff00";
	p."outline_line_color" = "#8bb0ea";

	(* Title *)
	p.title."align" = "left";
	p.title."text_color" = "#8bb0ea";
	p.title."text_font_size" = "14px";
	p.title."text_font" = "Open Sans";
	p.title."text_font_style" = "bold";
 
	(* AXIS Globals *)
	p.axis."minor_tick_in" = 0;
	p.axis."minor_tick_out" = 4;
	p.axis."major_tick_in" = 0;
	p.axis."major_tick_out" = 8;
 
 
	(* X-AXIS *)
	p.xaxis."axis_line_color" = "#8bb0ea";
	p.xaxis."major_label_orientation" = .8;
	p.xaxis."axis_label_text_color" = "#8bb0ea";
	p.xaxis."major_label_text_color" = "#8bb0ea";
	p.xaxis."axis_label_text_font_style" = "bold";
	p.xaxis."major_tick_line_color" = "#8bb0ea";
	p.xaxis."minor_tick_line_color" = "#8bb0ea";

	(* Y-AXIS *)
	p.yaxis."major_label_orientation" = .8;
	p.yaxis."major_label_text_color" = "#8bb0ea";
	p.yaxis."axis_line_color" = "#8bb0ea";
	p.yaxis."axis_label_text_color" = "#8bb0ea";
	p.yaxis."axis_label_text_font_style" = "bold";
	p.yaxis."major_tick_line_color" = "#8bb0ea";
	p.yaxis."minor_tick_line_color" = "#8bb0ea";

	(* X-GRID *)
	p.xgrid."grid_line_color" = "#8bb0ea";
	p.xgrid."grid_line_width" = .5;

	(* Y-GRID *)
	p.ygrid."grid_line_width" = .5;
	p.ygrid."grid_line_color" = "#8bb0ea";

	p.legend.location = "bottom_right";
	p.legend."click_policy" = "hide";

	(* Toolbar on top of the chart *)
	p."toolbar_location" = "above";

	p
	]

defaultPieModelTheme[p_] :=
	Module[{},
	(* canvas BG *)
	p."background_fill_color" = "#ffffff00";
	p."border_fill_color" = "#ffffff00";
	p."outline_line_color" = "#8bb0ea";

	(* Title *)
	p.title."align" = "left";
	p.title."text_color" = "#8bb0ea";
	p.title."text_font_size" = "14px";
	p.title."text_font" = "Open Sans";
	p.title."text_font_style" = "bold";
 
	p
	]


ThemeClass.applytomodel[model_] :=
	Module[{},
		If[!IsInstanceQ[model, ModelClass], Return[Null]];
		Switch[o."json",
			{}, defaultModelTheme[model],
			"PieChart", defaultPieModelTheme[model],
			_, Null
		]
	]

(* ::Section:: *)
(* Events *)

Get["MBokeh`Core`Events`"]

(* ::Section:: *)
(* Globals *)

$BokehOutputFileName = "mbokeh.html"

OutputFile[fname_String] := 
	Module[{nfname = fname},
		If[StringFreeQ[nfname, "."],
			nfname = fname <> ".html" ];
		$BokehOutputFileName = nfname
	]
OutputFile[""] := OutputFile[]
OutputFile[] := ($BokehOutputFileName = "mbokeh.html")

IsInstanceQ[p_BagClass, BagClass]	:= True
IsInstanceQ[p : MBokehClassPatterns, class_:BokehAttributeClass ] :=
	Module[{supers = Append[Supers[p.type[]], p.type[]]},
		MemberQ[supers, class]
	]
IsInstanceQ[p : MBokehClassPatterns, classes_List] := ( Or @@ (IsInstanceQ[p,#]& /@ classes) )
IsInstanceQ[p : MBokehEventClassPatterns, class_:EventClass ] :=
	Module[{supers = Append[Supers[p.type[]], p.type[]]},
		MemberQ[supers, class]
	]
IsInstanceQ[p : MBokehEventClassPatterns, classes_List] := ( Or @@ (IsInstanceQ[p,#]& /@ classes) )
IsInstanceQ[_,___] := False

HasAttrQ[obj_, attr:(_String | _Symbol)] := MemberQ[obj.getFields[], GetSymbolName@attr]

SetAssociationField[p_[o_, ___], field_String, key_String, value_] := o[field][key] = value

$MasterBokehBag := $MasterBokehBag = New[BagClass][]

(* ::Section:: *)
(* Utilities *)

(* Classes traversal *)

Clear[selectorHelper]
selectorHelper[a_Association] 				:= a
selectorHelper[p : MBokehClassSymbols]		:= <|"type" -> p|>
selectorHelper[name_String]					:= <|"name" -> name|>
selectorHelper[opts : OptionsPattern[] ]	:= Association[opts]

(*
selectorHelper[<||>]
selectorHelper[TitleClass]
selectorHelper["mycircle"]
selectorHelper["name" -> "foo", "type" -> HoverToolClass]
*)

Clear[find]
find[l_List, None, _] := l;
find[l_List, selector_Association, context_: None] :=
	Module[{type, ldata, nselect, keys, values, getAttrs},
		ldata = find[l, context, None];
		type = If[KeyExistsQ[selector,"type"], selector["type"], None];
		ldata = If[type === None, ldata, Select[ldata, IsInstanceQ[#, type] &]];
		nselect = KeyDrop[selector, "type"];
		keys = Keys[nselect];
		values = Values[nselect];
		getAttrs[p_] := (p.#) & /@ keys;
		Select[ldata, (getAttrs[#] === values) &]
  ]

Clear[ResetBokehInstancesFlag]
ResetBokehInstancesFlag[] :=
	Module[{instances},
		instances = $MasterBokehBag.all[];
		Scan[(#.$flagged = False)&, instances];
	]

myFlatt0[p_BaseClass] 	:= 
	myFlatt0 /@ Part[p,1]
myFlatt0[p: MBokehClassPatterns ] := 
	p.attributeID[]
myFlatt0[p_BagClass]		:= 
	(#.attributeID[]) & /@ (p.all[])
myFlatt0[x : (_List | _Association)] := 
	myFlatt0 /@ x
myFlatt0[x_] := 
	x

myFlatt[p_BaseClass] 	:=  
	myFlatt0[p]
myFlatt[p_BagClass]		:= 
	myFlatt0[p]
myFlatt[p: Alternatives @@ (Blank /@ MBokehClasses) ] := 
	KeyTake[ myFlatt0 /@ Part[p, 1], {"attributes", "id", "subtype", "type"}]
myFlatt[x : (_List | _Association)] := 
	myFlatt0[x]
myFlatt[x_] := 
	myFlatt0[x]

$tracehead = LegendClass
	
Clear[sowBokehClasses]
sowBokehClasses[b_BagClass, reset_:False, trace_:False] := 
	Module[{},
(*		
		If[trace, Print[b.all[]]];
*)
		Scan[sowBokehClasses[#, reset, trace] &, (b.all[])]
	]
sowBokehClasses[a_List, reset_:False, trace_:False] :=
	Module[{},
		Scan[sowBokehClasses[#, reset, trace] &, a ]
	]	
sowBokehClasses[a_Association, reset_:False, trace_:False] :=
	Module[{},
		Scan[sowBokehClasses[#, reset, trace] &, Values[a]]
	]
sowBokehClasses[p : MBokehClassPatterns, reset_:False, trace_:False] := 
	Module[{fields},
		If[MissingQ[p.$flagged] || !(p.$flagged),
(*
	If[trace || Head[p] === $tracehead, Print[Head[p]] ];
	If[trace || Head[p] === $tracehead, Print[Part[p.attributes,1]]];
*)
			p.updateAttributes[reset];
(*			
	If[trace || Head[p] === $tracehead, Print[Part[p.attributes,1]]];
*)									
			fields = Values@Part[p.attributes, 1];
(*
	If[trace || Head[p] === $tracehead, Print[fields]];
*)
			(p.$flagged) = True;
(*			
			Scan[sowBokehClasses[#, False, trace || Head[p] === $tracehead] &, fields];
*)
			Scan[sowBokehClasses[#, False, False] &, fields];
			Sow[p]
		];
	]
sowBokehClasses[p_,___] := Null

collectBokehClasses[p : MBokehClassPatterns, reset_:False] := 
Module[{},
	ResetBokehInstancesFlag[];
 	Union[ Flatten@Last@Reap[sowBokehClasses[p, reset]],
  		SameTest -> ((#1).attributeID[] == (#2).attributeID[] &)]
]

  
(* ::Section:: *)
(* BokehClasses *)

ResetClasses[]

(* ::Section:: *)
(* BokehAttributeClass *)
(* BaseClass *)

SetAttributes[setClassField, HoldFirst]
setClassField[p: _[o_Symbol,___], name_, BaseClass] := 
	(p.name = New[BaseClass][]; )
setClassField[p: _[o_Symbol,___], name_, c: Alternatives @@ MBokehClasses ] := 
	(p.name = New[c][])
setClassField[p: _[o_Symbol,___], name_, List[BaseClass]] := 
	(p.name = New[BagClass][]; )
setClassField[p: _[o_Symbol,___], name_, List[c: Alternatives @@ MBokehClasses] ] := 
	(p.name = New[BagClass][])
setClassField[p: _[o_Symbol,___], name_, (fname_String)[v_, opts___]] := 
	(p.name = v)

(* Check first for key name, if not found look for method *)
getAttributeMethod[p : _[object_Symbol,___], key_] :=
	Module[{skey = GetSymbolName@key, res},
		res = object[skey];
		If[MissingQ[res] && MemberQ[p.getAllFunctions[], skey],
			res = p.key[]
			];
		res
  ]

BokehAttributeClass /: (p : BokehAttributeClass[object_Symbol, ___]).(key : Except[sub | super | this, _Symbol | _String]) := 
	getAttributeMethod[p, key]

BokehAttributeClass = 
	NewClass[
		"Parents"	-> {BaseClass},
		"Fields"	-> {
			"type"			-> None,
			"attributes"	-> BaseClass,
			Options			-> {}
			}
	]
	
BokehAttributeClass.init[opts : OptionsPattern[BokehAttributeClass] ] := 
	Module[{},
		$MasterBokehBag.stuff[o];
		o.$flagged		= False;
		o.$Locked		= False;
		o.$FailedInit	= False;
		o.id			= BokehGetID[];
		o.Options		= o.optionValue[opts, Options];
		o.initClasses[];
	]

BokehAttributeClass.initClasses[]			:=
	Module[{optnames, class},
		class = Head[o];
		optnames = Part[Options[class],All,1];
		Scan[setClassField[o, #, o.#]&, optnames];
	]
	
BokehAttributeClass.getAllFunctions[] := Union@Flatten[
  GetFunctions[o.type[]][#] & /@ 
   Append[DeleteCases[Supers[o.type[]], BaseClass], o.type[]]]
   
BokehAttributeClass.resetAttributes[]		:= (o.attributes = New[BaseClass][])
BokehAttributeClass.updateAttributes[reset_:False] :=
	Module[{class, refclass, attrnames, templated, nvalue},
		If[(o.$Locked) === True, Return[]];
		If[reset || MissingQ[o.attributes], o.resetAttributes[]];
		class = Head[o];
		refclass = $BokehClassAttributes.class;
		If[MissingQ[refclass], refclass = New[class][]];
		attrnames = $BokehAttributeNames.class;
		If[MissingQ[attrnames],
			attrnames = Sort@Complement[
					Part[Options[class],All,1],
								{"type","subtype","attributes","Options","theme"}
			]
		];
		
		(* Apply theme, a theme has the highest precedence *)
		If[IsInstanceQ[o, ModelClass] && ((o."theme").type[])===ThemeClass,
			(o."theme").applytomodel[o]
		];
		
		templated = $BokehClassAttributesTemplates.class;
		If[!MissingQ[templated], (* then *)
			Scan[If[o.# =!= Part[templated.#,2] (* refclass.# *),
				nvalue = First@Map[Part[templated.#,1], {o.#}];
				If[nvalue === None, nvalue=Null ]; (* for JSON, None interpreted as Null *)
				Switch[nvalue,
					_BagClass,
						If[nvalue.len[]>0,
							o.attributes.# = nvalue
						],
					_,
						o.attributes.# = nvalue
				]
			]&,
			attrnames], (* else *)

			Scan[If[o.# =!= refclass.#, 
				o.attributes.# = o.#
				]&, attrnames]
		];
	]

BokehAttributeClass.tableform[opts___]						:= TableForm[Transpose[{o.getFields[], o.getFieldValues[]}], opts]
	
BokehAttributeClass.optionValue[opts_,name_]				:= OptionValue[Head[o], opts, name]
BokehAttributeClass.normalize[reset_:False, lock_:False]	:= (o.updateAttributes[reset]; If[lock, p.$Locked=True]; myFlatt[o])
BokehAttributeClass.rawjson[depth_: 4] 						:= ExportString[o.normalize[], "RawJSON", "Compact" -> depth]
BokehAttributeClass.attributeID[] 							:= <| "id" -> o.id, "type"->o.type |>
BokehAttributeClass.references[reset_:False]				:= collectBokehClasses[o, reset]

(* serialization *)
(* We want to evaluate all attrs only once, 
so we lock after the references collection *)

BokehAttributeClass.serializeInstances[reset_:False] := 
	Module[{refs},
		refs = o.references[reset];
		Scan[(#.$Locked = True)&, refs];
		(* should we onlock after ?? *)
		(#.normalize[])& /@ refs
	]
BokehAttributeClass.serializeJSON[depth_:4] := 
	ExportString[o.serializeInstances[], "RawJSON", "Compact" -> depth]

BokehAttributeClass.inspect[cname_String] :=
	Module[{attrs, tnames, fname},
		attrs = o.serializeInstances[];
		tnames = Union@Sort[#["type"] & /@ attrs];		
		fname = If[MemberQ[tnames, cname], cname, First@tnames];
		Manipulate[
			With[{pattrs=Select[attrs, (#["type"] === type) &]},
				
				ExportString[#, "RawJSON"] & /@ Reverse@pattrs
			],
			{{type, fname ,"Type: "}, tnames}
		]
	]
BokehAttributeClass.inspect[cname_Symbol] := o.inspect[SymbolName[cname]]
BokehAttributeClass.inspect[] := o.inspect[""]


MBokehUnlock[p : MBokehClassPatterns] := 
	Module[{refs},
		refs = p.references[reset];
		Scan[(#.$Locked = False)&, refs];
	]
MBokehUnlock[] := 
		Scan[(#.$Locked = False)&, $MasterBokehBag]
		
MBokehClearAll[] := 
	Module[{},
		MBokeh`Private`$MasterBokehBag.reset[];
	]
	
$BokehAttributeNames := $BokehAttributeNames = 
Module[{res},
	res = New[BaseClass][];
	Scan[(res.# = Sort@Complement[
					Part[Options[#],All,1],
								{"type","subtype","attributes","Options","theme"} 
				])& , MBokehClasses];
	res
]

getClassAttrs[p_Symbol] :=
	Module[{res, names, opts},
		res = New[BaseClass][];
		names = $BokehAttributeNames.p;
		opts = Options[p];
		Scan[(res.#=(# /. opts))&, names];
		res
	]

$BokehClassAttributes := $BokehClassAttributes = 
Module[{res},
	res = New[BaseClass][];
	Scan[(res.# = getClassAttrs[#])& , MBokehClasses];	
	res
]

getClassAttrsTemplates[p_Symbol] :=
	Module[{res, attrs, clsname},
		clsname = StringReplace[ToString[p], "Class" -> ""];
		res = New[BaseClass][];
		attrs = $BokehClassAttributes.p;
		Scan[With[{opts={"attrname"->#, "class"->p, "classname"->clsname}},
			(res.# = attrs.# /. {
				tf_String[val_, o___] -> {
					 Function[ToExpression["MBokeh`Private`"<>tf][#,Sequence@@Join[{o},Prepend[opts,"default"->val]]]], 
					 val,
					 Join[{o},Prepend[opts,"default"->val]],
					 ToExpression[tf]
					 },
				a_-> { Function[#], a , Prepend[opts,"default"->a], Identity}
				}
			)]&, attrs.getFields[]
		];
		res
	]

$BokehClassAttributesTemplates := $BokehClassAttributesTemplates = 
Module[{res},
	res = New[BaseClass][];
	Scan[(res.# = getClassAttrsTemplates[#])& , MBokehClasses];
	res
]


(* ::Section:: *)
(* Bokeh Classes *)


(* ModelClass *)
Get["MBokeh`Core`Model`"]
(* TransformClass *)
Get["MBokeh`Core`Transform`"]
(* Tile Sources *)
Get["MBokeh`Core`Tiles`"]
(* TickerClass *)
Get["MBokeh`Core`Tickers`"]
(* TickFormatterClass *)
Get["MBokeh`Core`Formatters`"]
(* LayoutDOMClass *)
Get["MBokeh`Core`Layouts`"]
(* Widgets *)
Get["MBokeh`Core`Widgets`"]
(* RenderClass *)
Get["MBokeh`Core`Renderers`"]
(* AxisModelClass *)
Get["MBokeh`Core`Axes`"]
(* AnnotationModelClass *)
Get["MBokeh`Core`Annotations`"]
(* Tools *)
Get["MBokeh`Core`Tools`"]
(* ScaleClass *)
Get["MBokeh`Core`Scales`"]
(* RangeClass *)
Get["MBokeh`Core`Ranges`"]
(* GlyphClass *)
Get["MBokeh`Core`Glyphs`"]
(* CallbackClass *)
Get["MBokeh`Core`Callback`"]
(* DataSource *)
Get["MBokeh`Core`Sources`"]
(* PlotModelClass *)
Get["MBokeh`Core`Plots`"]
(* Helpers *)
Get["MBokeh`Core`Helpers`"]
(* Figure *)
Get["MBokeh`Core`Figure`"]
(* GMap *)
Get["MBokeh`Core`MapPlots`"]

(* Typesetting *)
Get["MBokeh`Core`MBokehTypesetting`"]

(* DisplayFunction *)
Get["MBokeh`Core`DisplayFunction`"]

(* BokehGraph *)
Get["MBokeh`Core`BokehGraph`"]

End[]

(* ::Section:: *)
(* Protect class constructors wrappers *)

$ProtectClasses[$safeSymbols]

(* ::Section:: *)
(* Protect Symbols *)

SetAttributes[{#}, {ReadProtected}]& /@ MBokehClasses

SetAttributes[{#}, {ReadProtected, Protected}]& /@ $MBokehClassesPublic
SetAttributes[{#}, {ReadProtected, Protected}]& /@ $MBokehMethods
Remove[$MBokehClassesPublic]
Remove[$MBokehMethods]

EndPackage[]

Global`PackageTimingEnd[]
