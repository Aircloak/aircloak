// @flow

import React from "react";
import ReactCSSTransitionGroup from "react-addons-css-transition-group";

import {QueryView} from "./query";
import type {Query} from "./query";

const renderQueries = (queries: Query[]) => {
  if (queries.length > 0) {
    return queries.map((query) =>
      <QueryView key={query.id} query={query} />
    );
  } else {
    return (
      <tr>
        <td colSpan="5">
          Currently there are no queries running.
        </td>
      </tr>
    );
  }
};

export const QueriesView = (props: {queries: Query[]}) =>
  <div>
    <h3>Queries</h3>
    <table className="table">
      <thead>
        <tr>
          <th>Data source</th>
          <th>Cloak</th>
          <th>Analyst</th>
          <th>Query</th>
          <th>State</th>
          <th></th>
          <th></th>
        </tr>
      </thead>
      <ReactCSSTransitionGroup
        component="tbody"
        transitionName="activity-monitor-queries"
        transitionEnterTimeout={500}
        transitionLeaveTimeout={300}
      >
        {renderQueries(props.queries)}
      </ReactCSSTransitionGroup>
    </table>
  </div>;
