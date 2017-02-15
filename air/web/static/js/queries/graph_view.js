// @flow

import React from "react";
import {Bar} from "react-chartjs-2";

const data = (graphData) => {
  return {
    labels: graphData.x(),
    datasets: graphData.series(),
  };
};

const options = (graphConfig) => {
  return {
    scales: {
      xAxes: [{
        scaleLabel: {
          display: true,
          labelString: graphConfig.xColumns().join(", "),
        },
      }],
    },
  };
};

export const GraphView = (props: Props) => {
  if (props.graphData.ready()) {
    return (
      <Bar
        data={data(props.graphData)}
        options={options(props.graphConfig)}
        height={props.height}
        width={props.width}
        redraw={true}
      />
    );
  } else {
    return <div className="alert alert-warning">Select at least one X and Y axis.</div>
  }
}
