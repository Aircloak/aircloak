// @flow

import React from "react";
import _ from "lodash";

import {StateView} from "./state_view";
import {QueryView} from "./query";
import type {Authentication} from "../request";
import type {Query} from "./query";

const MAX_QUERIES_TO_SHOW = 20;

const renderQueries = (queries: Query[], authentication: Authentication) => {
  if (queries.length > 0) {
    return queries.slice(0, MAX_QUERIES_TO_SHOW).map((query) =>
      <QueryView key={query.id} query={query} authentication={authentication} />
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

const renderQueryStatsBreakdown = (queries: Query[]) => {
  const queryStats = {};
  queries.forEach(query => {
    if (queryStats[query.state]) {
      queryStats[query.state] += 1;
    } else {
      queryStats[query.state] = 1;
    }
  });
  return (
    <div>
      <h4>Snapshot of current query states</h4>

      <table className="table">
        <thead>
          <tr>
            <th>State</th>
            <th>Number of queries in state</th>
          </tr>
        </thead>
        <tbody>
          {_.map(queryStats, (count, queryState) =>
            <tr key={queryState}>
              <td><StateView state={queryState} /></td>
              <td>{count}</td>
            </tr>
          )}
        </tbody>
      </table>
    </div>
  );
};

const renderNumActiveQueriesShown = (queries: Query[]) => {
  const numQueries = queries.length;
  if (numQueries > MAX_QUERIES_TO_SHOW) {
    return (
      <div>
        {renderQueryStatsBreakdown(queries)}

        <p>
          Showing the <strong>{MAX_QUERIES_TO_SHOW}</strong> most recent
          of the <strong>{numQueries}</strong> currently active queries.
        </p>
      </div>
    );
  } else {
    return null;
  }
};

export class QueriesView extends React.PureComponent {
  render() {
    return (
      <div>
        <h3>Queries</h3>
        {renderNumActiveQueriesShown(this.props.queries)}
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
          <tbody>
            {renderQueries(this.props.queries, this.props.authentication)}
          </tbody>
        </table>
      </div>
    );
  }
}
