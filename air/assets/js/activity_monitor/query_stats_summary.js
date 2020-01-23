// @flow

import React from "react";
import _ from "lodash";

import type {Query} from "./query";
import StateView from "./state_view";

type Props = {
  queries: Query[]
}

export default (props: Props) => {
  const queryStats = {};
  const {queries} = props;
  queries.forEach((query) => {
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
          {_.map(queryStats, (count, queryState) => (
            <tr key={queryState}>
              <td><StateView queryState={queryState} /></td>
              <td>{count}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
