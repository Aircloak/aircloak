// @flow

import React from "react";
import ReactDOM from "react-dom";
import _ from "lodash";

import {QueriesView} from "./queries";
import type {Query} from "./query";

import {QuerySocket} from "../query_socket";

type QueryEvent = {
  query_id: string,
  event: string,
  query: Query,
};

type Props = {
  userId: number,
  guardianToken: string,
  CSRFToken: string,
  querySocket: QuerySocket,
  queries: Query[],
};

class ActivityMonitorView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {
      queries: this.props.queries,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);

    this.props.querySocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
  }

  state: {
    queries: Query[],
  };
  handleQueryEvent: (queryEvent: QueryEvent) => void;
  keepCompleted: (query: Query[], n: number) => Query[];

  keepCompleted(queries, n) {
    let completedKept = 0;
    return _.filter(queries, (query) => {
      if (query.state === "completed") {
        if (completedKept >= 10) {
          return false;
        } else {
          completedKept = completedKept + 1;
        }
      }
      return true;
    });
  }

  handleQueryEvent(queryEvent) {
    let queries = _.cloneDeep(this.state.queries);

    if (queryEvent.event === "started") {
      queries.unshift(queryEvent.query);
    } else {
      queries = _.map(queries, (existingQuery) => {
        if (existingQuery.id === queryEvent.query_id) {
          const alteredQuery = _.clone(existingQuery);
          alteredQuery.state = queryEvent.event;
          return alteredQuery;
        } else {
          return existingQuery;
        }
      });
    }

    this.setState({queries: this.keepCompleted(queries, 10)});
  }

  render() {
    return (
      <div>
        <QueriesView queries={this.state.queries} />
      </div>
    );
  }
}

export default function renderACtivityMonitorView(data: Props, elem: Node) {
  const socket = new QuerySocket(data.guardianToken);
  ReactDOM.render(<ActivityMonitorView querySocket={socket} {...data} />, elem);
}
