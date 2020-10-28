// @flow

import type { Element } from "React";
import React from "react";

import type { Query } from "./query";
import StateView from "./state_view";
import { allStates } from "../queries/state.js";

type Props = {
  queries: Query[],
};

const queryStats = (queries: Query[]): { [string]: number } => {
  const queryStats: { [string]: number } = {};
  queries.forEach((query) => {
    if (queryStats[query.state]) {
      queryStats[query.state] += 1;
    } else {
      queryStats[query.state] = 1;
    }
  });
  return queryStats;
};

export default ({ queries }: Props): Element<"div"> => {
  const stateStats = queryStats(queries);
  return (
    <div>
      <h4>Snapshot of current query states</h4>

      <table className="table table-responsive">
        <thead>
          <tr>
            <th>State</th>
            <th>Number of queries in state</th>
          </tr>
        </thead>
        <tbody>
          {allStates.map((stateName) => {
            const numQueriesCount = stateStats[stateName] || 0;
            return (
              <tr key={stateName}>
                <td>
                  <StateView queryState={stateName} />
                </td>
                <td>{numQueriesCount}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
};
