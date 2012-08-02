var tmp;

draw_histogram = function(info) {
  var monthnum = function(t) {
    return t.getFullYear() * 12 + t.getMonth();
  };
  var hist = d3.layout.histogram()
    .value(function(ci) { return monthnum(ci.stamp); })
    .bins(monthnum(info.commits[info.commits.length-1].stamp) - monthnum(info.commits[0].stamp) + 1)
    .bins(d3.range(monthnum(info.commits[0].stamp),
                   monthnum(info.commits[info.commits.length-1].stamp) + 1
                ))
  ;
  var stack = d3.layout.stack();
  var layers = info.data.map(function(repo) {
    return hist(repo.commits);
  });
  stack(layers);

  var m = [20, 20, 30, 20],
      w = 680 - m[1] - m[3],
      h = 320 - m[0] - m[2],
      dx = 2;

  var x = d3.scale.linear()
    .range([0, w - 60])
    .domain([monthnum(info.commits[0].stamp), monthnum(info.commits[info.commits.length-1].stamp)])
  ;
  var y = d3.scale.linear()
    .range([h, 0])
    .domain([0, d3.max(layers[layers.length-1].map(function(e) {return e.y + e.y0;}))])
  ;


  var svg = d3.select("#histogram")
    .append("svg:svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", h + m[0] + m[2])
    .append("svg:g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

  var xAxisScale = d3.time.scale()
    .range([0, w - 60])
    .domain([
      new Date(info.commits[0].stamp.getFullYear(), info.commits[0].stamp.getMonth()),
      new Date(info.commits[info.commits.length-1].stamp.getFullYear(), info.commits[info.commits.length-1].stamp.getMonth())
    ])
  ;
  var xAxis = d3.svg.axis()
                    .scale(xAxisScale)
                    //.ticks(d3.time.months, 3)
                    .orient("bottom");

  svg.append("svg:g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + h + ")")
      .call(xAxis);

  var yAxis = d3.svg.axis()
                    .scale(y)
                    //.ticks(8)
                    //.tickSize(6)
                    .orient("left");

  svg.append("svg:g")
      .attr("class", "y axis")
      .attr("transform", "translate(" + m[1] + ",0)")
      .call(yAxis);

  var svglayers = svg.selectAll("g.layer")
    .data(layers)
    .enter()
      .append("g")
        .attr("class", "layer")
        .style("fill", function(d, i) { return info.repoColorScale(i); })
        .style("fill-opacity", "0.42")
  ;

  var bars = svglayers.selectAll("g.bar")
    .data(function(d) { return d; })
    .enter()
      .append("g")
        .attr("class", "bar")
      .append("svg:rect")
        .attr("x", function(d) { return x(d.x); })
        .attr("width", function(d) { return x(d.x + d.dx) - x(d.x) - dx; })
        .attr("y", function(d) { return y(d.y0 + d.y);})
        .attr("height", function(d) { return y(d.y0) - y(d.y0 + d.y); })
  ;
}

draw_dow_count = function(info) {
  //      0          lm                              rm                       w
  // 0    *    day    * 1 2 3 .....................23 * axis                  w
  // hm   * -- Mon -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      * -- Tue -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      * -- Wed -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      * -- Thu -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      * -- Fri -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      * -- Sat -- * --------- heatmap ----------- * -- horizontalHistPerDay
  // bm   * -- Sun -- * --------- heatmap ----------- * -- horizontalHistPerDay
  //      *             ---- verticalHistPerHour ----
  //      *             ---- verticalHistPerHour ----
  // h    *             ---- verticalHistPerHour ----

  var lm = 30,
      w = 680,
      rm = w - 90,
      hm = 20,
      h = 280,
      bm = h - 70,
      xm = 2,
      ym = 2;

  var week_x = d3.scale.linear()
    .range([0, lm])
    .domain([0, 1])
  ;

  var hm_x = d3.scale.linear()
    .range([lm, rm])
    .domain([0, 24])
  ;

  var hh_x = d3.scale.linear()
    .range([rm, w])
    .domain([0, 1])
  ;

  var hm_y = d3.scale.linear()
    .range([hm, bm])
    .domain([0, 7])
  ;

  var svg = d3.select("#dowcount")
    .append("svg:svg")
      .attr("width", w)
      .attr("height", h);

  var heatmap = svg
    .append("svg:g");

  var days = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat","Sun"];
  heatmap.selectAll("g.day")
    .data(d3.range(7))
    .enter()
      .append("svg:g")
        .attr("class", "day")
      .append("svg:text")
        .attr("x", 0)
        .attr("y", function(d) { return hm_y(d) + (hm_y(1) - hm_y(0))/2; })
        .attr("alignment-baseline", "middle")
        .text(function(d) { return days[d]; });

  heatmap.selectAll("g.cheader")
    .data(d3.range(24))
    .enter()
      .append("svg:g")
        .attr("class", "cheader")
      .append("svg:text")
        .attr("text-anchor", "middle")
        .attr("x", function(e) { return hm_x(e) + (hm_x(1) - hm_x(0))/2; })
        .attr("y", hm_y(0) - ym)
        .text(function(d) { return d; });

  var color = d3.interpolateRgb("rgb(255,230,230)", "rgb(255,0,0)");
  var maxCommit = 0;
  d3.range(7).forEach(function(_,i) {
    d3.range(24).forEach(function(_,j) {
      c = info.dowcount.d[i][j];
      if (c > maxCommit) {
        maxCommit = c;
      }
    });
  });
  heatmap.selectAll("g.day").each(function(day) {
    d3.select(this).selectAll("g.hour")
      .data(function(d) { return d3.range(24); })
      .enter()
        .append("svg:g")
          .attr("class", "hour");
    d3.select(this).selectAll("g.hour")
        .append("svg:rect")
          .attr("fill", function(h) {
              // TODO scale!
              cin = info.dowcount.d[day][h] / maxCommit;
              return color(cin);
           })
          .attr("fill-opacity", "0.85")
          .attr("x", function(h) { return hm_x(h); })
          .attr("width", hm_x(1) - hm_x(0) - xm)
          .attr("height", hm_y(1) - hm_y(0) - ym)
          .attr("y", hm_y(day))
    ;
  });

  // horizontal bar
  var maxCommitPerDay = 0;
  info.dowcount.d.forEach(function(d, i) {
    var c = info.dowcount.sum_for_day(i);
    if (c > maxCommitPerDay) {
      maxCommitPerDay = c;
    }
  });
  heatmap.selectAll("g.day")
    .append("svg:rect")
      .attr("fill", function(d) { return 40; })
      .attr("fill-opacity", "0.5")
      .attr("x", rm)
      .attr("y", function(day) { return hm_y(day);})
      .attr("width", function(day) { 
         return info.dowcount.sum_for_day(day) * (w - rm) / maxCommitPerDay;
      })
      .attr("height", hm_y(1) - hm_y(0) - ym)
  ;
  heatmap.selectAll("g.day")
      .append("svg:text")
        .attr("x", function(day) { 
           return rm + info.dowcount.sum_for_day(day) * (w - rm) / maxCommitPerDay - xm;
        })
        .attr("y", function(day) { return hm_y(day) + (hm_y(1) - hm_y(0))/2;})
        .attr("text-anchor", "end")
        .text(function(d) { return info.dowcount.sum_for_day(d); });
  ;

  var vh = svg .append("svg:g");

  var maxCommitPerHour = 0.0;
  d3.range(24).forEach(function(d, i) {
    var c = info.dowcount.sum_for_hour(i);
    if (c > maxCommitPerHour) {
      maxCommitPerHour = +c;
    }
  });
  vh.selectAll("g.hour")
    .data(d3.range(24))
    .enter()
      .append("svg:g")
        .attr("class", "hour")
      .append("svg:rect")
        .attr("fill", function(d) { return 40; })
        .attr("fill-opacity", "0.5")
        .attr("x", function(h) { return hm_x(h);})
        .attr("y", bm)
        .attr("width", hm_x(1) - hm_x(0) - xm)
        .attr("height", function(hour) {
          return (info.dowcount.sum_for_hour(hour) / maxCommitPerHour) * (h - bm);
        })
  ;
  vh.selectAll("g.hour")
    .append("svg:text")
      .attr("x", function(h) { return hm_x(h);})
      .attr("y", bm - (hm_x(1) - hm_x(0))/2 + xm)
      .text(function(d) { return info.dowcount.sum_for_hour(d); })
      .attr("transform", function(h) {
        return "rotate(90 "+hm_x(h) + " " + bm +")";
        //return "translate("+hm_x(h)+" "+bm+")rotate(90)";
      })
      .attr("alignment-baseline", "middle")
  ;
};

draw_stacks = function(info) {
  var m = [20, 20, 30, 20],
      w = 680 - m[1] - m[3],
      h = 320 - m[0] - m[2];

  var svg = d3.select("#commithist").append("svg:svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", h + m[0] + m[2])
    .append("svg:g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

  var x = d3.time.scale()
    .range([0, w - 60])
    .domain([ info.commits[0].stamp
            , info.commits[info.commits.length - 1].stamp
            ]
    )
  ;

  var y = d3.scale.linear()
    .range([h, 0])
    .domain([0, info.commits.length])
  ;

  var make_area = function(idx) {
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(info.commits[d].stamp);
      })
      .y0(function(d) {
        return y(idx == 0 ? 0 : info.stacks[idx-1][d].ci);
      })
      .y1(function(d) {
        return y(info.stacks[idx][d].ci);
      });
    return area(d3.range(info.commits.length));
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
    .data(info.data)
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
          return info.repoColorScale(i);
        })
        .style("fill-opacity", 0.5);
    e.append("svg:text")
     .attr("x", w - 60)
     .attr("y", function(d) {
        var lastCi = info.commits.length - 1;
        var y0 = i == 0 ? 0 : info.stacks[i-1][lastCi].ci;
        var y1 = info.stacks[i][lastCi].ci;
        return y((y0 + y1) / 2);
     })
     .text(d.repo);
  });




  var svg = d3.select("#linechangedhist").append("svg:svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", h + m[0] + m[2])
    .append("svg:g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

  var y = d3.scale.linear()
    .range([h, 0])
    .domain([0, info.stacks[info.stacks.length - 1][info.commits.length - 1].linechanged()])
  ;

  var make_area = function(idx) {
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(info.commits[d].stamp);
      })
      .y0(function(d) {
        return y(idx == 0 ? 0 : info.stacks[idx-1][d].linechanged());
      })
      .y1(function(d) {
        return y(info.stacks[idx][d].linechanged());
      });
    return area(d3.range(info.commits.length));
  }

  var yAxis = d3.svg.axis()
                    .scale(y)
                    //.ticks(8)
                    //.tickSize(6)
                    .orient("left");

  var g = svg.selectAll("g")
    .data(info.data)
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
          return info.repoColorScale(i);
        })
        .style("fill-opacity", 0.5);
    e.append("svg:text")
     .attr("x", w - 60)
     .attr("y", function(d) {
        var lastCi = info.commits.length - 1;
        var y0 = i == 0 ? 0 : info.stacks[i-1][lastCi].linechanged();
        var y1 = info.stacks[i][lastCi].linechanged();
        return y((y0 + y1) / 2);
     })
     .text(d.repo);
  });
  
  
  
  
  
  var svg = d3.select("#linecounthist").append("svg:svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", h + m[0] + m[2])
    .append("svg:g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

  var y = d3.scale.linear()
    .range([h, 0])
    .domain([0, info.stacks[info.stacks.length - 1][info.commits.length - 1].linecount()])
  ;

  var make_area = function(idx) {
    var area = d3.svg.area()
      .interpolate("basis")
      .x(function(d) {
        return x(info.commits[d].stamp);
      })
      .y0(function(d) {
        return y(idx == 0 ? 0 : info.stacks[idx-1][d].linecount());
      })
      .y1(function(d) {
        return y(info.stacks[idx][d].linecount());
      });
    return area(d3.range(info.commits.length));
  }

  var yAxis = d3.svg.axis()
                    .scale(y)
                    //.ticks(8)
                    //.tickSize(6)
                    .orient("left");

  var g = svg.selectAll("g")
    .data(info.data)
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
          return info.repoColorScale(i);
        })
        .style("fill-opacity", 0.5);
    e.append("svg:text")
     .attr("x", w - 60)
     .attr("y", function(d) {
        var lastCi = info.commits.length - 1;
        var y0 = i == 0 ? 0 : info.stacks[i-1][lastCi].linecount();
        var y1 = info.stacks[i][lastCi].linecount();
        return y((y0 + y1) / 2);
     })
     .text(d.repo);
  });
};


d3.json("./data/commits.json", function(data) {

  var parse = d3.time.format("%Y-%m-%dT%H:%M:%SZ").parse;
  var info = {};

  // dow[7][24]: count per day and per hour
  var dowcount = {};
  dowcount.d = d3.range(7).map(function(dow) {
    return d3.range(24).map(function(_) {
      return 0;
    })
  });
  dowcount.add_stamp = function(stamp) {
    this.d[stamp.getDay()][stamp.getHours()] += 1;
  };
  dowcount.sum_for_day = function(d) {
    return this.d[d].reduce(function(a,e) { return a+e;});
  };
  dowcount.sum_for_hour = function(h) {
    var s = 0;
    d3.range(7).forEach(function(d,i) {
      s += dowcount.d[i][h];
    });
    return s;
  };

  var commits = new Array();
  data.forEach(function(repo, repoIdx) {
    repo.commits.forEach(function(c) {
      c.stamp = parse(c.stamp);
      dowcount.add_stamp(c.stamp);
    });
    repo.commits.sort(function(a,b) { return a.stamp - b.stamp; });
    commits.push.apply(commits, repo.commits.map(function(c) {
      return { 'repoIdx': repoIdx
             , 'stamp': c.stamp
             , 'la': c.la
             , 'ld': c.ld
             };
    })); // concat in place
  });
  commits.sort(function(a,b) { return a.stamp - b.stamp; });

  // Accumulate commit count
  // TODO created similar y0/y values to be able to plot
  // both stacked or unstacked version ... even if i our case
  // we always have: y0 = y - y[-1]
  var stacks = new Array();
  function StackPoint() {
    this.ci = 0;
    this.la = 0;
    this.ld = 0;
  }
  StackPoint.prototype.clone = function() {
    var c = new StackPoint();
    c.ci = this.ci;
    c.la = this.la;
    c.ld = this.ld;
    return c;
  }
  StackPoint.prototype.linechanged = function() {
    return this.la + this.ld;
  }
  StackPoint.prototype.linecount = function() {
    return this.la - this.ld;
  }
  var currentDataPoint = new Array();
  data.forEach(function(repo, repoIdx) {
    currentDataPoint[repoIdx] = new StackPoint();
    stacks.push(new Array());
  });
  commits.forEach(function(d, ci) {
    d3.range(d.repoIdx, data.length).forEach(function(i) {
      currentDataPoint[i].ci += 1;  // incr repo commit count and all following stacks
      currentDataPoint[i].la += d.la;  // incr count for all following stacks
      currentDataPoint[i].ld += d.ld;  // incr count for all following stacks
    });
    data.forEach(function(repo, ri) {
      stacks[ri].push(currentDataPoint[ri].clone());
    });
  });
  info.data = data;
  info.commits = commits
  info.stacks = stacks
  info.dowcount = dowcount;
  if (data.length <= 10) {
    info.repoColorScale = d3.scale.category10();
  } else if (data.length <= 20) {
    info.repoColorScale = d3.scale.category20();
  } else {
    var inter = d3.interpolateRgb("rgb(213,213,213)", "rgb(42,42,42)");
    info.repoColorScale = d3.scale.ordinal()
                            .domain(d3.range(0, data.length))
                            .range(d3.range(0, data.length).map(function(i) { return inter(i); }))
    ;
  }

  draw_histogram(info);
  draw_stacks(info);
  draw_dow_count(info);

  tmp = info;
  console.log(info);
});

