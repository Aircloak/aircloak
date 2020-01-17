// @flow

import React from "react";
import {Bar} from "react-chartjs-2";
import _ from "lodash";

import type {GraphDataT, Series} from "./graph_data";

import type {Column} from "./result";

type Options = {
  scales: {
    yAxes: {ticks: {beginAtZero: boolean}}[],
    xAxes: {
      ticks: {beginAtZero: boolean, maxTicksLimit: number},
      scaleLabel: {
        display: boolean,
        labelString: string
      },
      gridLines: {
        display: boolean
      }
    }[]
  }
}

type Data = {
  labels: Column[],
  datasets: (Series & {
    backgroundColor: string
  }) []
};

type Props = {
  data: Data,
  options: Options
}

type State = {
  propsCache: Props,
  redraw: boolean,
};

const fillColors = [
  "rgba(170, 100, 100, 0.4)",
  "rgba(148, 193, 26, 0.4)",
  "rgba(30, 185, 214, 0.4)",
  "rgba(0, 170, 150, 0.4)",
];

const maxTicksShown = 20;

const data = (graphData) => ({
  labels: graphData.x(),
  datasets: graphData.series().map((series) => _.merge(series, {backgroundColor: fillColors[series.indexInResult % fillColors.length]})),
});

const options = (graphData): Options => ({
  scales: {
    yAxes: [{
      ticks: {
        beginAtZero: true,
      },
    }],

    xAxes: [{
      ticks: {
        beginAtZero: true,
        maxTicksLimit: maxTicksShown,
      },
      scaleLabel: {
        display: true,
        labelString: graphData.xLabel(),
      },
      gridLines: {
        display: false,
      },
    }],
  },
});

class BarWrapper extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      propsCache: props,
      redraw: true,
    };
  }

  static getDerivedStateFromProps(nextProps: Props, prevState: State) {
    if (_.isEqual(nextProps, prevState.propsCache)) {
      return {redraw: false};
    } else {
      return {redraw: true, propsCache: nextProps};
    }
  }

  render() {
    // A workaround for chart.js not refreshing some parts of the graph.
    // Setting redraw=true also doesn't work as the graph is then animated on
    // every UI change, even unrelated ones, like typing in the editor.
    // The clone is needed, because chart.js seems to modify the data passed to it.
    return <Bar {... _.cloneDeep(this.props)} redraw={this.state.redraw} />;
  }
}

export const GraphView = (props: {graphData: GraphDataT}) => {
  if (props.graphData.ready()) {
    return <BarWrapper data={data(props.graphData)} options={options(props.graphData)} />;
  } else {
    return <div className="alert alert-warning">Select at least one X and Y axis.</div>;
  }
};
