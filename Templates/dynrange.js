// Callback for xrange/yrange update
var tag 	= '`tag`';
var widget	= '`widget`';

var cb = function(r) {
			if(r.msg_type == 'execute_result') {
				if(r.content == 'Completed') {
					source.data.x[0]  = r.x0;
					source.data.y[0]  = r.y0;
					source.data.dw[0] = r.width;
					source.data.dh[0] = r.height;
					source.data.image[0] = Uint32Array.from(r.result);
					source.change.emit();
                    
					if (Bokeh.MBokeh.`ctx`.`name`._queued.length) {
						update_plot();
					} else {
						Bokeh.MBokeh.`ctx`.`name`._blocked = false;
					}
					Bokeh.MBokeh.`ctx`.`name`._timeout = Date.now();
				} else if(r.content == 'noctx') {
					console.log('MBokeh error: No data context in server');
					// TODO: Handle re-rendering of widget to recreate data in server.
				} else {
					console.log('MBokeh error:', r.content);
				}
			} else {
				console.log('MBokeh error: hm_askwidget returned unexpected message:', r)            
			}
		}
        
function update_plot() {
	var range = Bokeh.MBokeh.`ctx`.`name`._queued;
	// Execute the command on the Server side
		TenTen.App.hm_askwidget(tag, widget,
                                {
								 fname:'DispatchDynamicImage',
                                 callback:'`callback`',
                                 reset:`reset`,
                                 jsonargs:range}, 'json', cb);
		Bokeh.MBokeh.`ctx`.`name`._queued = [];
	}
        
var plot = xrange.plots[0];

// Generate a command to execute in Server	
var ranges = {
			xmin: xrange.start,
			ymin: yrange.start,
			xmax: xrange.end,
			ymax: yrange.end,
			w: Math.floor(plot.width),
			h: Math.floor(plot.height)}
var range_str = JSON.stringify(ranges)
        
if (typeof Bokeh.MBokeh === 'undefined') {
	Bokeh.MBokeh = {}
}
       
if (typeof Bokeh.MBokeh.`ctx` === 'undefined') {          
	Bokeh.MBokeh.`ctx` = {}
}

if (typeof Bokeh.MBokeh.`ctx`.`name` === 'undefined') {
	Bokeh.MBokeh.`ctx`.`name` = {}
}

if (!Bokeh.MBokeh.`ctx`.`name`._queued) {
	Bokeh.MBokeh.`ctx`.`name`._queued = [];
	Bokeh.MBokeh.`ctx`.`name`._blocked = false;
	Bokeh.MBokeh.`ctx`.`name`._timeout = Date.now();
}

timeout = Bokeh.MBokeh.`ctx`.`name`._timeout + `timeout`;
if(typeof TenTen === 'undefined') {
} else if ((Bokeh.MBokeh.`ctx`.`name`._blocked && (Date.now() < timeout))) {
	Bokeh.MBokeh.`ctx`.`name`._queued = [range_str];
} else {
	Bokeh.MBokeh.`ctx`.`name`._queued = [range_str];
	setTimeout(update_plot, `delay`);
	Bokeh.MBokeh.`ctx`.`name`._blocked = true;
	Bokeh.MBokeh.`ctx`.`name`._timeout = Date.now();
}
