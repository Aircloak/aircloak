// @flow

import React from "react";
import {Bar} from "react-chartjs-2";
import _ from "lodash";

import {GraphConfig} from "./graph_data";
import type {GraphDataT} from "./graph_data";

const fillColors = [
  "rgba(170, 100, 100, 0.1)",
  "rgba(148, 193, 26, 0.1)",
  "rgba(30, 185, 214, 0.1)",
  "rgba(0, 170, 150, 0.1)",
];

const borderColors = [
  "rgba(170,100,100, 0.4)",
  "rgba(148,193,26, 0.4)",
  "rgba(30,185,214, 0.4)",
  "rgba(0,170,150, 0.4)",
];

const data = (graphData) => ({
  labels: graphData.x(),
  datasets: graphData.series().map((series, i) =>
    _.merge(series, {
      backgroundColor: fillColors[i % fillColors.length],
      borderColor: borderColors[i % borderColors.length],
      borderWidth: 2,
    })
  ),
});

const options = (graphConfig) => ({
  scales: {
    xAxes: [{
      ticks: {
        beginAtZero: true,
      },
      scaleLabel: {
        display: true,
        labelString: graphConfig.xColumns().join(", "),
      },
    }],
  },
});

export const GraphView = (props: {graphData: GraphDataT, graphConfig: GraphConfig}) => {
  if (props.graphData.ready()) {
    return <Bar data={data(props.graphData)} options={options(props.graphConfig)} redraw />;
  } else {
    return <div className="alert alert-warning">Select at least one X and Y axis.</div>;
  }
};
