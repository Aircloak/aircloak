// @flow

import React from "react";
import {Bar} from "react-chartjs-2";
import _ from "lodash";

import {GraphConfig} from "./graph_data";
import type {GraphDataT} from "./graph_data";

const fillColors = [
  "rgba(170, 100, 100, 0.4)",
  "rgba(148, 193, 26, 0.4)",
  "rgba(30, 185, 214, 0.4)",
  "rgba(0, 170, 150, 0.4)",
];

const data = (graphData) => ({
  labels: graphData.x(),
  datasets: graphData.series().map((series, i) =>
    _.merge(series, {backgroundColor: fillColors[i % fillColors.length]})),
});

const options = (graphConfig) => ({
  scales: {
    yAxes: [{
      ticks: {
        beginAtZero: true,
      },
    }],
    xAxes: [{
      ticks: {
        beginAtZero: true,
      },
      scaleLabel: {
        display: true,
        labelString: graphConfig.xColumns().join(", "),
      },
      gridLines: {
        display: false,
      },
    }],
  },
});

class BarWrapper extends React.Component {
  constructor(props) {
    super(props);
    this.state = {redraw: true};
  }

  state: {redraw: boolean};

  componentWillReceiveProps(nextProps) {
    this.setState({redraw: ! _.isEqual(nextProps, this.props)});
  }

  render() {
    // A workaround for chart.js not refreshing some parts of the graph.
    // Setting redraw=true also doesn't work as the graph is then animated on
    // every UI change, even unrelated ones, like typing in the editor.
    // The clone is needed, because chart.js seems to modify the data passed to it.
    return <Bar {... _.cloneDeep(this.props)} redraw={this.state.redraw} />;
  }
}

export const GraphView = (props: {graphData: GraphDataT, graphConfig: GraphConfig}) => {
  if (props.graphData.ready()) {
    return <BarWrapper data={data(props.graphData)} options={options(props.graphConfig)} />;
  } else {
    return <div className="alert alert-warning">Select at least one X and Y axis.</div>;
  }
};
