// @flow

import type { Element } from "React";
import React from "react";

import { QueryView } from "./query";
import QueryStatsSummaryView from "./query_stats_summary";
import type { Query } from "./query";

type Props = {
  queries: Query[],
};

const MAX_QUERIES_TO_SHOW = 20;

const renderQueries = (queries: Query[]) => {
  if (queries.length > 0) {
    return queries
      .slice(0, MAX_QUERIES_TO_SHOW)
      .map((query) => <QueryView key={query.id} query={query} />);
  } else {
    return (
      <tr>
        <td colSpan="5">Currently there are no queries running.</td>
      </tr>
    );
  }
};

const renderNumActiveQueriesShown = (queries: Query[]) => {
  const numQueries = queries.length;
  if (numQueries > MAX_QUERIES_TO_SHOW) {
    return (
      <div>
        <QueryStatsSummaryView queries={queries} />

        <p>
          {"Showing the "}
          <strong>{MAX_QUERIES_TO_SHOW}</strong>
          {" most recent of the "}
          <strong>{numQueries}</strong>
          {" currently active queries."}
        </p>
      </div>
    );
  } else {
    return null;
  }
};

export default class QueriesView extends React.PureComponent<Props> {
  render(): Element<"div"> {
    const { queries } = this.props;
    return (
      <div>
        {renderNumActiveQueriesShown(queries)}
        <table className="table table-responsive-lg">
          <thead>
            <tr>
              <th>Data source</th>
              <th>Cloak</th>
              <th>Analyst</th>
              <th>Query</th>
              <th>State</th>
              <th> </th>
              <th> </th>
            </tr>
          </thead>
          <tbody>{renderQueries(queries)}</tbody>
        </table>
      </div>
    );
  }
}
