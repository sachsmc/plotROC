function clickForCis(idstr){
		d3.selectAll("[id^=\'" + idstr + "\'] rect").attr("opacity", 0)
		d3.selectAll("[id^=\'" + idstr + "\'] use").attr("opacity", 0)
		d3.selectAll("[id^=\'" + idstr + "\'] text").attr("opacity", 0)

		var rocdata = []

		d3.selectAll("[id^=\'" + idstr + "\'] use").each(function(d, i){
  
			me = d3.select(this);
			rocdata.push(
				{"x": me.attr("x"), 
				"y": me.attr("y")}
			);
  
		})

		var voronoi = d3.geom.voronoi()
		.x(function(d){ return d.x;})
		.y(function(d){ return d.y;})
		.clipExtent([[-2, -2], [720 + 2, 720 + 2]]);

		var tess = voronoi(rocdata)

		var rocdata2 = [];
		for(var i = 0; i < rocdata.length; i++){
  
			rocdata[i].vtess = tess[i];
			if(rocdata[i].vtess == undefined){ continue; } else {
    
				rocdata2.push(rocdata[i]);
	
			}
		} 

		var svg = d3.select("g#testgridSVG");
		var cells = svg.append("g").attr("class", "vors").selectAll("g");		

		cell = cells.data(rocdata2);
		cell.exit().remove();

		var cellEnter = cell.enter().append("g").attr("class", "vor");

		cellEnter.append("path")
		.attr("class", "tess")
		;

		cell.select("path").attr("d", function(d) { return "M" + d.vtess.join("L") + "Z"; });   


		svg.selectAll(".vor").on("click", function(d, i){

			d3.selectAll("[id^=\'" + idstr + "\'] rect").attr("opacity", 0);
			d3.selectAll("[id^=\'" + idstr + "\'] rect:nth-child(" + (i + 1) + ")").attr("opacity", 1);

			d3.selectAll("[id^=\'" + idstr + "\'] use").attr("opacity", 0);
			d3.selectAll("[id^=\'" + idstr + "\'] use:nth-child(" + (i + 1) + ")").attr("opacity", 1);

			d3.selectAll("[id^=\'" + idstr + "\'] text").attr("opacity", 0);
			d3.selectAll("[id^=\'" + idstr + "\'] text")
			.select(function(e, j){ if(j == i){ 
				return this;
			} else {
				return null;
			}
		}).attr("opacity", 1);

	})
}