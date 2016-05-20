import React from "react";
import ReactDOM from "react-dom";

const QueriesView = (props) =>
  <div>Queries</div>;

export default function renderQueriesView(data, elem) {
  ReactDOM.render(<QueriesView {...data} />, elem);
};
