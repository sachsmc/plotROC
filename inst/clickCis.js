function clickForCis(idstr){
		d3.selectAll("[id^=\'" + idstr + "\'] rect").attr("opacity", 0).attr("clicked", "false")
		d3.selectAll("[id^=\'" + idstr + "\'] use").attr("opacity", 0).attr("clicked", "false")
		d3.selectAll("[id^=\'" + idstr + "\'] text").attr("opacity", 0).attr("clicked", "false")

    //lookup table for matched points, rect, and text
    
    var gids = []
    d3.selectAll("[id^=\'" + idstr + "\'] > g").each(function(){ gids.push(this.id) });
    var lookup = {}

    for(var i=0; i < gids.length; i++){
      
      var ispoint = gids[i].match(/.*points.*/)
      if(ispoint !== null){
        //lookahead for rect and text
        var rectdex = null;
        var textdex = null;
        
        gids.slice(i + 1, i + 3).forEach(function(val){
          tmp = val.match(/.*rect.*/)
          if(tmp !== null){
            rectdex = tmp[0]
          }
        });
        gids.slice(i + 1, i + 3).forEach(function(val){
          tmp2 = val.match(/.*text.*/)
          if(tmp2 !== null){
            textdex = tmp2[0]
          }
        });
        lookup[ispoint[0]] = {"rect": rectdex, "text": textdex}
      }
      
    }

		var rocdata = []

		d3.selectAll("[id^=\'" + idstr + "\'] use").each(function(d, i){
  
			me = d3.select(this);
			// find corresponding rect and text
			
			tolook = me.attr("id").substring(0, me.attr("id").lastIndexOf("."))
			atend = me.attr("id").substring(me.attr("id").lastIndexOf("."), me.attr("id").length)
			
			rocdata.push(
				{"x": me.attr("x"), 
				"y": me.attr("y"), 
				  "pid": me.attr("id"), 
				  "rid": me.attr("id").replace(tolook, lookup[tolook]["rect"]), 
				  "tid": me.attr("id").replace(tolook, lookup[tolook]["text"]) 
				}
			);
  
		})

    // extract prefix from string
    
    var prefix = idstr.substring(0, idstr.indexOf("geom"))
		var svg = d3.select("g#" + prefix + "gridSVG");
		
		// find dims from panel background
		
		var dims = d3.select("[id=\'" + idstr + "\']").select(function(){ return this.parentNode })
		.select("[id^=\'" + prefix + "panel.background..rect\']").node().getBBox();
		
		var voronoi = d3.geom.voronoi()
		.x(function(d){ return d.x;})
		.y(function(d){ return d.y;})
		.clipExtent([[dims.x, dims.y], [dims.width + dims.x, dims.height + dims.y]]);

		var tess = voronoi(rocdata)

		var rocdata2 = [];
		for(var i = 0; i < rocdata.length; i++){
  
			rocdata[i].vtess = tess[i];
			if(rocdata[i].vtess === undefined){ continue; } else {
    
				rocdata2.push(rocdata[i]);
	
			}
		} 

    
		var cells = svg.append("g").attr("class", "vors").selectAll("g");		

		cell = cells.data(rocdata2);
		cell.exit().remove();

		var cellEnter = cell.enter().append("g").attr("class", "vor" + idstr.match(/[0-9]+/)[0]);

		cellEnter.append("path")
		.attr("class", "tess")
		;

		cell.select("path").attr("d", function(d) { return "M" + d.vtess.join("L") + "Z"; });   


		svg.selectAll(".vor" + idstr.match(/[0-9]+/)[0]).on("click", function(d, i){
		  
			d3.selectAll("[id^=\'" + idstr + "\'] rect").attr("opacity", 0).attr("clicked", "false");
			d3.selectAll("[id=\'" + d.rid + "\']").transition().duration(100).attr("opacity", 1).attr("clicked", "true");

			d3.selectAll("[id^=\'" + idstr + "\'] use").attr("opacity", 0).attr("clicked", "false");
			d3.selectAll("[id=\'" + d.pid + "\']").attr("opacity", 1).attr("clicked", "true");

			d3.selectAll("[id^=\'" + idstr + "\'] text *").attr("opacity", 0).attr("clicked", "false");
			d3.selectAll("[id=\'" + d.tid + "\'] *").attr("opacity", 1).attr("clicked", "true");

	})
	
	.on("mouseover", function(d, i){
		  
		  
			d3.selectAll("[id^=\'" + idstr + "\'] use").selectAll("[clicked=\'false\']").attr("opacity", 0);
			d3.selectAll("[id=\'" + d.pid + "\']").attr("opacity", 1);

			d3.selectAll("[id^=\'" + idstr + "\'] text *").selectAll("[clicked=\'false\']").attr("opacity", 0);
			d3.selectAll("[id=\'" + d.tid + "\'] *").attr("opacity", 1);

	})
	.on("mouseout", function(d, i){
	  d3.selectAll("[id=\'" + d.pid + "\']").attr("opacity", function(){
	    if(d3.select(this).attr("clicked") == "true"){ 
	      return 1;
	    } else {
	      return 0;
	    }
	  })
	  
	  d3.selectAll("[id=\'" + d.tid + "\'] *").attr("opacity", function(){
	    if(d3.select(this).attr("clicked") == "true"){ 
	      return 1;
	    } else {
	      return 0;
	    }
	  })
	})
	
	
}
