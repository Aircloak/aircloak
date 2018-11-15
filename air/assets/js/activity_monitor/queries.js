// @flow

import React from "react";
import TransitionGroup from "react-transition-group/TransitionGroup";
import CSSTransition from "react-transition-group/CSSTransition";

import {QueryView} from "./query";
import type {Authentication} from "../request";
import type {Query} from "./query";

const renderQueries = (queries: Query[], authentication: Authentication) => {
  if (queries.length > 0) {
    return queries.map((query) =>
      <CSSTransition
        key={query.id}
        classNames="activity-monitor-query"
        timeout={{enter: 500, exit: 300}}
      >
        <QueryView query={query} authentication={authentication} />
      </CSSTransition>
    );
  } else {
    return (
      <CSSTransition
        key="no-queries"
        classNames="activity-monitor-query"
        timeout={{enter: 500, exit: 300}}
      >
        <tr>
          <td colSpan="5">
            Currently there are no queries running.
          </td>
        </tr>
      </CSSTransition>
    );
  }
};

export class QueriesView extends React.PureComponent {
  render() {
    return (
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
          <TransitionGroup component="tbody">
            {renderQueries(this.props.queries, this.props.authentication)}
          </TransitionGroup>
        </table>
      </div>
    );
  }
}
