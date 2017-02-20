// @flow

import React from "react";
import ReactCSSTransitionGroup from "react-addons-css-transition-group";

import {QueryView} from "./query";
import type {Query} from "./query";

const hasQueries = (queries: Query[]) => queries.length > 0;

export const QueriesView = (props: {queries: Query[]}) => {
  let queries = "";
  if (hasQueries(props.queries)) {
    queries = props.queries.map((query) =>
      <QueryView key={query.id} {...query} />
    );
  } else {
    queries = (
      <tr>
        <td colSpan="3">
          Currently there are no queries running.
        </td>
      </tr>
    );
  }

  return (
    <div>
      <h3>Queries</h3>
      <table className="table">
        <thead>
          <tr>
            <th>Data source</th>
            <th>Analyst</th>
            <th>Query state</th>
          </tr>
        </thead>
        <ReactCSSTransitionGroup
          component="tbody"
          transitionName="activity-monitor-queries"
          transitionEnterTimeout={500}
          transitionLeaveTimeout={300}
        >
          {queries}
        </ReactCSSTransitionGroup>
      </table>
    </div>
  );
};
