// @flow

import React from "react";

import type { Query } from "./query";
import StateView from "./state_view";

type Props = {
  queries: Query[],
};

const queryStats = (queries: Query[]) => {
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

export default ({ queries }: Props) => {
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
          {Object.entries(queryStats(queries)).map(
            ([queryState: string, count: number]) => {
              // Object.entries returns an array of [string, mixed] values.
              // We know the mixed values to all be number, but the only way to
              // convince Flow of this is by doing a type refinement:
              if (typeof count !== "number") {
                return <React.Fragment />;
              }
              return (
                <tr key={(queryState: string)}>
                  <td>
                    <StateView queryState={(queryState: string)} />
                  </td>
                  <td>{count}</td>
                </tr>
              );
            }
          )}
        </tbody>
      </table>
    </div>
  );
};
