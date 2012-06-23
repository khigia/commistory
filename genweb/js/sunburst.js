var width = 800,
    height = width,
    radius = Math.min(width, height) / 2,
    delta = radius / 6,
    color = d3.scale.category20c();

var vis = d3.select("#chartsunburst").append("svg")
    .attr("width", width)
    .attr("height", height)
  .append("g")
    .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

var partition = d3.layout.partition()
    .sort(null)
    .size([2 * Math.PI, radius * radius])
    //.size([2 * Math.PI, radius])
    .value(function(d) { return d.size; });

var arc = d3.svg.arc()
    .startAngle(function(d) { return d.x; })
    .endAngle(function(d) { return d.x + d.dx; })
    .innerRadius(function(d) { return Math.sqrt(d.y) - (d.depth ? delta : 0); })
    .outerRadius(function(d) { return Math.sqrt(d.y + d.dy) - (d.depth ? delta : 0); });
    //.innerRadius(function(d) { return (d.y); })
    //.outerRadius(function(d) { return (d.y + d.dy); });

function make(nodes) {
  vis.selectAll("path").remove();
  vis.selectAll("text").remove();
  
  var path = vis.data(nodes).selectAll("path")
      .data(partition.nodes)
    .enter().append("path")
      //.attr("display", function(d) { return d.depth ? null : "none"; }) // hide inner ring
      .attr("d", arc)
      .attr("fill-rule", "evenodd")
      .style("stroke", "#fff")
      .style("fill", function(d) { return color((d.children ? d : d.parent).name); })
      .on("click", click)
      .each(stash);

  var text = vis.data(nodes).selectAll("text").data(partition.nodes);
  var textEnter = text.enter().append("text")
        .style("fill-opacity", 1)
        .style("fill", function(d) {
          return "#000";
        })
        .attr("text-anchor", function(d) {
          return (d.x + d.dx / 2) > Math.PI ? "end" : "start";
        })
        //.attr("dy", ".2em")
        .attr("transform", function(d) {
          var angle = (d.x + d.dx / 2) * 180 / Math.PI - 90;
          return "rotate(" + (d.depth ? angle : 0) + ")translate(" + (Math.sqrt(d.y) - (d.depth ? delta : 0)) + ")rotate(" + (angle > 90 ? -180 : 0) + ")";
        })
        .text(function(d) { return (d.depth < 10) ? d.name : ""; });
        ;
  //textEnter.append("tspan")
  //    .attr("x", function(d) { return d.x;})
  //    .text(function(d) { return d.name; });
  //textEnter.append("tspan")
  //    .attr("x", 0)
  //    .attr("dy", "1em")
  //    .text(function(d) { return d.name; });

  function click(d) {
    if (d.parent) {
      console.log(d);
      make([d.depth ? d : d.parent]);
      // path
      //     .data(partition.value(function(e) { return e.depth; }))
      //   .transition()
      //   .duration(1500)
      //   .attrTween("d", arcTween);
    }
  };
}

d3.json("./data/fs.json", function(json) {
  console.log(json);
  make([json]);
});

// Interpolate the arcs in data space.
function arcTween(a) {
  var i = d3.interpolate({x: a.x0, dx: a.dx0}, a);
  return function(t) {
    var b = i(t);
    a.x0 = b.x;
    a.dx0 = b.dx;
    return arc(b);
  };
}
function stash(d) {
  d.x0 = d.x;
  d.dx0 = d.dx;
}
