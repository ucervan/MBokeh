(* Wolfram Language package *)

(* ::Section:: *)
(* WebDriver *)

MBDisplayFunction[gr_] :=
	Module[{height, width, imgsize, p, im, opts = $DisplayFuncOpts},		
		(* ImageSize *)
		imgsize = ImageSize /. opts;
		imgsize = Switch[imgsize,
					_Integer, {imgsize, imgsize},
					{_,_}, imgsize,
					_, {550,550}
					];
		{width, height} = imgsize;
		opts = Prepend[opts, ImageSize->imgsize];
		
		im = Rasterize[gr];
		p = figure["title" -> "MBokeh", 
			"x_range" -> {0, 10}, "y_range" -> {0, 10}, 
			"plot_height" -> height, "plot_width" -> width,
			"tools" -> "pan,box_zoom,wheel_zoom,reset,save"];
		p.imagergba["image" -> {im}, "x" -> {0}, "y" -> {0}, "dw" -> {10}, "dh" -> {10}];

		p.toolbar."logo" = None;
		p.xaxis."visible" = False;
		p.yaxis."visible" = False;

		show[p, opts]
  	 ]

MBokehDisplayFunction[] := 
	If[($WebDriverQ && ($WebSession =!= None || $WebSessionHeadless =!= None)), MBDisplayFunction, Identity]
