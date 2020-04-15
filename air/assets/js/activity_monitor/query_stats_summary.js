// @flow

import React from "react";

import type { Query } from "./query";
import StateView from "./state_view";

type Props = {
  queries: Query[],
};

const queryStats = (queries) => {
  const queryStats = {};
  queries.forEach((query) => {
    if (queryStats[query.state]) {
      queryStats[query.state] += 1;
    } else {
      queryStats[query.state] = 1;
    }
  });
  return queryStats;
};

export default ({ queries }: Props) => {
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
          {Object.entries(queryStats(queries)).map(([count, queryState]) => (
            <tr key={(queryState: any)}>
              <td>
                <StateView queryState={(queryState: any)} />
              </td>
              <td>{count}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
