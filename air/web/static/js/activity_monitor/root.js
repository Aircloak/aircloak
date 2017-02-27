// @flow

import React from "react";
import ReactDOM from "react-dom";
import _ from "lodash";

import {QueriesView} from "./queries";
import type {Query} from "./query";

import {QuerySocket} from "../query_socket";
import {isFinished} from "../queries/state";
import {Disconnected} from "../disconnected";

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

    // How long to display a query after it has completed
    this.queryRemovalTime = 10000; // 10 seconds

    this.state = {
      queries: this.props.queries,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);
    this.handleRemoveQuery = this.handleRemoveQuery.bind(this);

    this.props.querySocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
  }

  state: {
    queries: Query[],
  };
  queryRemovalTime: number;
  handleQueryEvent: (queryEvent: QueryEvent) => void;
  handleRemoveQuery: (query: Query) => void;

  handleRemoveQuery(queryId) {
    const queries = _.reject(this.state.queries, (query) => query.id === queryId);
    this.setState({queries});
  }

  conditionallyScheduleQueryRemoval(queryEvent) {
    if (isFinished(queryEvent.event)) {
      setTimeout(() => this.handleRemoveQuery(queryEvent.query_id), this.queryRemovalTime);
    }
  }

  handleQueryEvent(queryEvent) {
    let queries = _.cloneDeep(this.state.queries);

    this.conditionallyScheduleQueryRemoval(queryEvent);

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

    this.setState({queries});
  }

  render() {
    return (
      <div>
        <Disconnected socket={this.props.querySocket} />
        <QueriesView queries={this.state.queries} />
      </div>
    );
  }
}

export default function renderACtivityMonitorView(data: Props, elem: Node) {
  const socket = new QuerySocket(data.guardianToken);
  ReactDOM.render(<ActivityMonitorView querySocket={socket} {...data} />, elem);
}
