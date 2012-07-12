<ul>
  <li>Author summary table (active day, commit number, line added/deleted)</li>
  <li>Line history per author (iff can remove filter-out path)</li>
  <li>TreeMap of all file, each file further split by % of git blame line, each square size proportional to size, colored by author</li>
</ul>

<h2>Summary</h2>
<div class="row">
  <div class="span8">
    <table class="table">
      <thead>
        <tr><th>Author</th>
            <th>Commits</th>
            <th>+lines</th>
            <th>-lines</th>
            <th>First/Last(Age)</th>
            <th>Active days</th>
        </tr>
      </thead>
      <tbody>
        <tr><td>TODO</td>
            <td>TODO</td>
            <td>TODO</td>
            <td>TODO</td>
            <td>TODO</td>
            <td>TODO</td>
        </tr>
      </tbody>
    </table>
  </div>
</div>


<h2>Commit history</h2>

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

  var commits = new Array();
  data.forEach(function(repo) {
    commits.push.apply(commits, repo.commits);
  });

  data = {'commits': commits};
  console.log(data);
  tmp = data;

  var parse = d3.time.format("%Y-%m-%dT%H:%M:%SZ").parse;
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

  // stack: accumulate commit count accross author
  data.commits.forEach(function(_d, ci) {
    for (var ai in d3.range(authors.length - 1)) {
      data.matrix[1*ai + 1][ci] += data.matrix[ai][ci];
    }
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
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(data.commits[d].stamp);
      })
      .y0(function(d) {
        return y(idx == 0 ? 0 : data.matrix[idx-1][d]);
      })
      .y1(function(d) {
        return y(data.matrix[idx][d]);
      });
    return area(d3.range(data.commits.length));
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
    .data(authors)
    .enter()
      .append("svg:g")
      .attr("class", "author");

  svg.append("svg:g")
      .attr("class", "y axis")
      .attr("transform", "translate(" + m[1] + ",0)")
      .call(yAxis);

  svg.append("svg:g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + h + ")")
      .call(xAxis);

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
        var lastCi = data.commits.length - 1;
        var y0 = idx == 0 ? 0 : data.matrix[idx-1][lastCi];
        var y1 = data.matrix[idx][lastCi];
        return y((y0 + y1) / 2);
     })
     .text(d);
  });

});

</script>
