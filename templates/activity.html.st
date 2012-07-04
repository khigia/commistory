<ul>
  <li>Commit history per projects and global</li>
</ul>

<div id="charthist"></div>
<script type="text/javascript">

var tmp;

var m = [20, 20, 30, 20],
    w = 960 - m[1] - m[3],
    h = 500 - m[0] - m[2];

var x,
    y;

var color = d3.scale.category10();

var svg = d3.select("#charthist").append("svg:svg")
    .attr("width", w + m[1] + m[3])
    .attr("height", h + m[0] + m[2])
  .append("svg:g")
    .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

d3.json("./data/commits.json", function(data) {

  console.log(data);

  // TODO for now simplify to first repo
  data = {'commits':data[0].commits};
  console.log(data);

  var parse = d3.time.format("%Y-%m-%d %H:%M:%S").parse;
  data.commits.forEach(function(c) { c.stamp = parse(c.stamp); });
  data.commits.sort(function(a,b) { return a.stamp - b.stamp; });

  // Create a list of authors and index from author name to index
  var authors = new Array();
  var authorIdx = {}; // name -> idx
  data.commits.forEach(function(d) {
    if (! authorIdx.hasOwnProperty(d.author)) {
      authorIdx[d.author] = authors.length;
      authors.push(d.author);
    }
  });
  console.log("author", authorIdx);

  // Accumulate commit count per author
  var currentDataPoint = new Array();
  data.matrix = new Array();
  for (var i in d3.range(authors.length)) {
    currentDataPoint[i] = 0;
    data.matrix.push(new Array());
  }
  data.commits.forEach(function(d, i) {
    currentDataPoint[authorIdx[d.author]] += 1;
    currentDataPoint.forEach(function(cnt, j) {
      data.matrix[j].push(cnt);
    });
  });
  console.log("data", data);


  x = d3.time.scale()
    .range([0, w - 60])
    .domain([ data.commits[0].stamp
            , data.commits[data.commits.length - 1].stamp
            ]
    )
  ;

  y = d3.scale.linear()
    .range([h, 0])
    .domain([0, data.commits.length])
  ;

  var make_area = function(idx) {
    console.log("idx", idx);
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(data.commits[d].stamp);
      })
      .y0(function(d) {
        if (idx == 0) {
          return y(0);
        } else {
          return y(d3.range(idx)
                     .map(function(i) { return data.matrix[i][d]; })
                     .reduce(function(a,v) { return a + v; })
          );
        }
      })
      .y1(function(d) {
        // TODO accumulated can be done once for all before!
        return y(d3.range(idx + 1)
                   .map(function(i) { return data.matrix[i][d]; })
                   .reduce(function(a,v) { return a + v; })
        );
      });
    return area(d3.range(data.commits.length));
  }

  var g = svg.selectAll("g")
    .data(authors)
    .enter()
      .append("svg:g")
      .attr("class", "author");

  g.each(function(d) {
    var e = d3.select(this);
    e.append("svg:path", ".line")
        .attr("class", "area")
        .attr("d", make_area(authorIdx[d]))
        .style("fill", function(d) {
          return color(authorIdx[d]);
        })
        .style("fill-opacity", 0.5);
    e.append("svg:text")
     .attr("x", w - 60)
     .attr("y", function(d) {
        var idx = authorIdx[d];
        var y0, y1;
        if (idx == 0) {
          y0 = 0;
        } else {
          y0 = d3.range(idx)
                   .map(function(i) { return data.matrix[i][data.commits.length - 1]; })
                   .reduce(function(a,v) { return a + v; })
          ;
        }
        y1 = d3.range(idx + 1)
               .map(function(i) { return data.matrix[i][data.commits.length - 1]; })
               .reduce(function(a,v) { return a + v; })
        ;
        return y((y0 + y1) / 2);
     })
     .text(d);
  });
});

</script>
