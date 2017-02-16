// @flow

import React from "react";

import {QueryView} from "./query";
import type {Query} from "./query";

export const QueriesView = (props: {queries: Query[]}) => {
  let queries = "";
  if (props.queries.length === 0) {
    queries = (
      <tr>
        <td colSpan="3">
          Currently there are no queries running.
        </td>
      </tr>
    );
  } else {
    queries = props.queries.map((query, i) =>
      <QueryView key={i} {...query} />
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
