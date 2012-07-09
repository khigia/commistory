<ul>
  <li>Commit history per projects and global</li>
  <li>Line number history per projects and global (line added, deleted, and line count)</li>
</ul>

<div id="commithist"></div>
<script type="text/javascript">

var tmp;

var m = [20, 20, 30, 20],
    w = 960 - m[1] - m[3],
    h = 500 - m[0] - m[2];

var x,
    y;

var color = d3.scale.category10();

var svg = d3.select("#commithist").append("svg:svg")
    .attr("width", w + m[1] + m[3])
    .attr("height", h + m[0] + m[2])
  .append("svg:g")
    .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

d3.json("./data/commits.json", function(data) {

  var commits = new Array();

  var parse = d3.time.format("%Y-%m-%d %H:%M:%S").parse;
  data.forEach(function(repo, repoIdx) {
    repo.commits.forEach(function(c) { c.stamp = parse(c.stamp); });
    commits.push.apply(commits, repo.commits.map(function(c) { return {'repoIdx': repoIdx, 'stamp': c.stamp} })); // concat in place
  });
  commits.sort(function(a,b) { return a.stamp - b.stamp; });

  // Accumulate commit count
  var stacks = new Array();
  var currentDataPoint = new Array();
  data.forEach(function(repo, repoIdx) {
    currentDataPoint[repoIdx] = 0;
    stacks.push(new Array());
  });
  commits.forEach(function(d, ci) {
    d3.range(d.repoIdx, data.length).forEach(function(i) {
      currentDataPoint[i] += 1;  // incr repo commit count and all following stacks
    });
    data.forEach(function(repo, ri) {
      stacks[ri].push(currentDataPoint[ri]);
    });
  });
  tmp = {'data': data, 'commits': commits,' stacks': stacks};
  console.log(tmp);

  x = d3.time.scale()
    .range([0, w - 60])
    .domain([ commits[0].stamp
            , commits[commits.length - 1].stamp
            ]
    )
  ;

  y = d3.scale.linear()
    .range([h, 0])
    .domain([0, commits.length])
  ;

  var make_area = function(idx) {
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(commits[d].stamp);
      })
      .y0(function(d) {
        return y(idx == 0 ? 0 : stacks[idx-1][d]);
      })
      .y1(function(d) {
        return y(stacks[idx][d]);
      });
    return area(d3.range(commits.length));
  }

  var xAxis = d3.svg.axis()
                    .scale(x)
                    .ticks(d3.time.months, 3)
                    .orient("bottom");

  var yAxis = d3.svg.axis()
                    .scale(y)
                    //.ticks(8)
                    //.tickSize(6)
                    .orient("left");

  var g = svg.selectAll("g")
    .data(data)
    .enter()
      .append("svg:g")
      .attr("class", "repo");

  svg.append("svg:g")
      .attr("class", "y axis")
      .attr("transform", "translate(" + m[1] + ",0)")
      .call(yAxis);

  svg.append("svg:g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + h + ")")
      .call(xAxis);

  g.each(function(d,i) {
    var e = d3.select(this);
    e.append("svg:path", ".line")
        .attr("class", "area")
        .attr("d", make_area(i))
        .style("fill", function(d) {
          return color(i);
        })
        .style("fill-opacity", 0.5);
    e.append("svg:text")
     .attr("x", w - 60)
     .attr("y", function(d) {
        var lastCi = commits.length - 1;
        var y0 = i == 0 ? 0 : stacks[i-1][lastCi];
        var y1 = stacks[i][lastCi];
        return y((y0 + y1) / 2);
     })
     .text(d.repo);
  });

});

</script>
