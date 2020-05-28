import React from "react";
import * as d3 from "d3";
import { Popover, OverlayTrigger } from "react-bootstrap";
import AnalysisDetails from "./analysis_details";

const select = (item, options) => {
  for (const key of Object.keys(options)) {
    const keys = key.split(",");
    const match = item.analysis.filter((an) => key.includes(an.name));
    if (match.length === keys.length) {
      return options[key](
        ...keys.map((key) => match.find((m) => m.name === key).value)
      );
    }
  }
  return null;
};

const HorizontalBarChart = ({ data }) => {
  const w = 32;
  const h = 16;
  const x = d3
    .scaleLinear()
    .range([8, w - 8])
    .domain([0, d3.max(Object.values(data))]);
  const y = d3
    .scaleBand()
    .range([h, 0])
    .domain(Object.keys(data))
    .paddingInner(0.1);
  const color = d3
    .scaleOrdinal()
    .range(d3.schemeTableau10)
    .domain(Object.keys(data));

  return (
    <svg width={w} height={h}>
      {Object.entries(data).map(([label, datum]) => (
        <g key={label}>
          <text
            x={6}
            y={y(label) + y.bandwidth()}
            dy="-0.12em"
            fontSize={Math.ceil(y.bandwidth())}
            fontWeight="bold"
            fill={color(label)}
            textAnchor="end"
          >
            {label}
          </text>
          <rect
            x={x.range()[0]}
            width={x(datum)}
            y={y(label)}
            height={y.bandwidth()}
            fill={color(label)}
          />
        </g>
      ))}
    </svg>
  );
};

const fmt = d3.format(".0f");

const HistogramChart = ({ data, min, max }) => {
  const w = 32;
  const h = 16;
  let minLabel, maxLabel, tw;
  if (min && max) {
    minLabel = fmt(min);
    maxLabel = fmt(max);
    tw = Math.ceil(4.313 * Math.max(minLabel.length, maxLabel.length));
    console.log(tw);
  } else {
    minLabel = maxLabel = "";
    tw = 0;
  }

  const x = d3
    .scaleLinear()
    .range([tw, w - tw])
    .domain([
      d3.min(data, (d) => d.lowerBound),
      d3.max(data, (d) => d.lowerBound + d.bucketSize),
    ]);

  const y = d3
    .scaleLinear()
    .range([0, h])
    .domain([0, d3.max(data, (d) => d.count)]);

  return (
    <svg width={w} height={h}>
      <text x={0} y={h} dy="-0.12em" fontSize={8} textAnchor="start">
        {minLabel}
      </text>
      <line x1={tw} x2={w - tw} y1={h} y2={h} stroke="black" />
      {data.map((datum) => (
        <rect
          key={datum.lowerBound}
          x={x(datum.lowerBound) + 1 / window.devicePixelRatio}
          width={
            x(datum.lowerBound + datum.bucketSize) -
            x(datum.lowerBound) -
            1 / window.devicePixelRatio
          }
          y={h - y(datum.count)}
          height={y(datum.count)}
        />
      ))}

      <text x={w} y={h} fontSize={8} dy="-0.12em" textAnchor="end">
        {maxLabel}
      </text>
    </svg>
  );
};

const rebin = (data) => {
  // this is a really dumb method
  const reconst = data.flatMap((datum) =>
    Array.from(new Array(datum.count), (_) => datum.value)
  );
  return d3
    .histogram()
    .thresholds(5)(reconst)
    .map((bin) => ({
      lowerBound: bin.x0,
      count: bin.length,
      bucketSize: bin.x1 - bin.x0,
    }));
};

const Sparkline = ({ item }) => {
  // Hierarchy of visualizations:
  switch (item.type) {
    case "boolean":
      return select(item, {
        "distinct.top_values": (analysis) => (
          <HorizontalBarChart
            data={Object.fromEntries(
              analysis.map((o) => [o.value ? "T" : "F", o.value])
            )}
          />
        ),
      });
    case "integer":
      return select(item, {
        "distinct.values,refined_min,refined_max": (data, min, max) => (
          <HistogramChart data={rebin(data)} min={min} max={max} />
        ),
      });
    // case "real":
    //   return select(item, {
    //     "histogram.buckets,refined_min,refined_max": (data, min, max) => (
    //       <HistogramChart data={data} min={min} max={max} />
    //     ),
    //   });
    // case "date":
    //   return select(item, {
    //     "dates_linear.day": (data) => (
    //       <HistogramChart
    //         data={data.counts.map((d) => ({
    //           count: d.count,
    //           lowerBound: new Date(d.value).valueOf(),
    //           bucketSize: 24 * 60 * 60 * 1000,
    //         }))}
    //       />
    //     ),
    //   });
  }

  return <i className="fas fa-chart-line" style={{ width: "32px" }} />;
};

const SparklineOverview = ({ item }) => (
  <OverlayTrigger
    trigger="click"
    placement="left"
    rootClose={true}
    overlay={(props) => AnalysisDetails(item, props)}
  >
    <button className="btn btn-outline-secondary">
      <Sparkline item={item} />
    </button>
  </OverlayTrigger>
);

export default SparklineOverview;
