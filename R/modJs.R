#' Modify js to use custom name
#' @param name Name of the points grob
#' @param prefix Prefix of the svg
#' 
#' @keywords Internal


modJs <- function(name, prefix){
jsstring <- '<script>
  // extract data from points to bind
var rocdata = [];

d3.selectAll("[id^=\'%s%s.1.\']").each(function(d, i){
  
  me = d3.select(this);
  rocdata.push({"x": me.attr("x"), 
                "y": me.attr("y"),
                "cut": me.attr("cutoff")});
  
})

var voronoi = d3.geom.voronoi()
.x(function(d){ return d.x;})
.y(function(d){ return d.y;})
.clipExtent([[-2, -2], [720 + 2, 720 + 2]]);

var tess = voronoi(rocdata);

for(var i = 0; i < rocdata.length; i++){
  
  rocdata[i].vtess = tess[i];
  
} 

var svg = d3.select("g#%sgridSVG");
var cells = svg.append("g").attr("class", "vors").selectAll("g");		

cell = cells.data(rocdata);
cell.exit().remove();

var cellEnter = cell.enter().append("g");

cellEnter.append("circle")
.attr("class", "dot")
.attr("r", 3.5)
.attr("cx", function(d) { return d.x; })
.attr("cy", function(d) { return d.y; })
;

cellEnter.append("path")
.attr("class", "tess")
;

cell.select("path").attr("d", function(d) { return "M" + d.vtess.join("L") + "Z"; });   

cellEnter.append("g")
.attr("transform", function(d){ return "translate(" + (d.x+20) + "," + d.y + ")"; } )
.append("text").attr("class", "hidetext").attr("transform", "scale(1, -1)").attr("dy", "15px")
.text(function(d) { return "cutoff: " + Math.round(d.cut*10)/10;  });


</script>
  '

sprintf(jsstring, prefix, name, prefix)

}
  
