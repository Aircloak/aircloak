// @flow

import React from "react";

import {QueryView} from "./query";
import type {Query} from "./query";

export class QueriesView extends React.Component {
  constructor(props: {queries: Query[]}) {
    super(props);
  }

  render() {
    var queries = "";
    if (this.props.queries.length == 0) {
      queries = (
        <tr>
          <td colSpan="3">
            Currently there are no queries running.
          </td>
        </tr>
      );
    } else {
      queries = this.props.queries.map((query, i) => {
        return <QueryView key={i} {...query} />;
      })
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
  }
}
