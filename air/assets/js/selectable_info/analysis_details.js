import React from "react";
import { Popover } from "react-bootstrap";
import { VegaLite } from "react-vega";

const toHuman = (name) => {
  switch (name) {
    case "distinct.top_values":
      return "Distinct Top Values";
    case "distinct.total_count":
      return "Distinct Total";
    case "distinct.suppressed_count":
      return "Distinct Suppresed";
  }
};

const maybeChart = (name, data) => {
  switch (name) {
    case "distinct.top_values":
      return (
        <VegaLite
          width={180}
          padding={10}
          actions={false}
          spec={{
            mark: {
              type: "bar",
            },
            data: {
              values: data,
            },
            encoding: {
              y: {
                field: "value",
                type: "ordinal",
              },
              x: {
                field: "count",
                type: "quantitative",
              },
            },
          }}
        />
      );
    default:
      return JSON.stringify(data);
  }
};

const AnalysisDetails = (item, props) => {
  return (
    <Popover {...props}>
      <Popover.Title>{item.name} - Diffix Explorer Analysis</Popover.Title>
      <Popover.Content className="p-0">
        <ul className="list-group list-group-flush">
          {item.analysis.map(({ name, value }) => {
            return (
              <li className="list-group-item">
                <h5>{toHuman(name)}</h5>
                {maybeChart(name, value)}
              </li>
            );
          })}
        </ul>
      </Popover.Content>
    </Popover>
  );
};

export default AnalysisDetails;
