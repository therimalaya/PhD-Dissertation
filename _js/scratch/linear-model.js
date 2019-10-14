// !preview r2d3 data=mpg, options=list(color = "royalblue"), dependencies="_js/colorbrewer.js", viewer="internal"
//
// r2d3: https://rstudio.github.io/r2d3
//

var margin = {top: 0, right: width / 24, bottom: height / 30, left: 0};

svg
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .style("font-size", Math.min(width, height) / 60)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

r2d3.onRender(function(dta, svg, width, height, options) {
  
  // Convert strings to numbers.
  dta.forEach(function(d) {
    d.hwy = +d.hwy;
    d.cty = +d.cty;
    d.displ = +d.displ;
  });
  
  // Compute the extent of the data set in age and years.
  xMinMax = d3.extent(dta, function(d){
    return parseFloat(d.hwy);
  });
  yMinMax = d3.extent(dta, function(d){
    return parseFloat(d.cty);
  });
  rMinMax = d3.extent(dta, function(d){
    return parseFloat(d.displ);
  });

  xScale = d3.scaleLinear()
    .domain([xMinMax[0], xMinMax[1]])
    .range([0, width - 10]);
  yScale = d3.scaleLinear()
    .domain([yMinMax[0], yMinMax[1]])
    .range([0, height - 10]);
  rScale = d3.scaleLinear()
    .domain([rMinMax[0], rMinMax[1]])
    .range([2, 12]);
  cScale = d3.scaleOrdinal()
    .domain(["f", "4", "r"])
    .range(colorbrewer.Spectral[3]);
    
  
  
  circles = svg.selectAll(".dot")
    .data(dta)
    .enter()
    .append('circle')
      .attr('class', 'dot')
      .attr('cx', function(d) { return xScale(d.hwy); })
      .attr('cy', function(d) { return yScale(d.cty); })
      .attr('r', function(d) { return rScale(d.displ); })
      .attr('fill', options.color)
      .attr('opacity', 0.5)
      .attr('stroke', options.color);

  xAxis = d3v5.axisBottom(xScale);
  yAxis = d3v5.axisLeft(yScale);
  
  xAxisG = svg.append('g')
    .attr('id', 'xAxis')
    .attr('class', 'axis')
    .attr('stroke', 'grey');
  yAxisG = svg.append('g')
    .attr('id', 'yAxis')
    .attr('class', 'axis')
    .attr('stroke', 'grey');
  
  xAxisG.call(xAxis)
    .attr('transform', 'translate(0, '+ (height - 30) +')');
  yAxisG.call(yAxis)
    .attr('transform', 'translate('+ 30 +', 0)');
});