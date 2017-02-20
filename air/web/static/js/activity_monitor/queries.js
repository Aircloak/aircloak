// @flow

import React from "react";

import {QueryView} from "./query";
import type {Query} from "./query";

const hasQueries = (queries: Query[]) => queries.length > 0;

export const QueriesView = (props: {queries: Query[]}) => {
  let queries = "";
  if (hasQueries(props.queries)) {
    queries = props.queries.map((query, i) =>
      <QueryView key={i} {...query} />
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
        <tbody>
          {queries}
        </tbody>
      </table>
    </div>
  );
};
