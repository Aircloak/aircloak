// @flow

import React from "react";
import ReactDOM from "react-dom";
import _ from "lodash";

import {QueriesView} from "./queries";
import type {Query} from "./query";
import {CloaksView} from "./cloaks";
import type {Cloak} from "./cloak";

import {FrontendSocket} from "../frontend_socket";
import {isFinished} from "../queries/state";

type QueryEvent = {
  query_id: string,
  event: string,
  query: Query,
};

type Props = {
  userId: number,
  guardianToken: string,
  CSRFToken: string,
  frontendSocket: FrontendSocket,
  queries: Query[],
  cloaks: Cloak[],
};

class ActivityMonitorView extends React.Component {
  constructor(props: Props) {
    super(props);

    // How long to display a query after it has completed
    this.queryRemovalTime = 10000; // 10 seconds

    this.state = {
      queries: this.props.queries,
      cloaks: this.props.cloaks,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);
    this.handleRemoveQuery = this.handleRemoveQuery.bind(this);
    this.handleMemoryReading = this.handleMemoryReading.bind(this);

    this.props.frontendSocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
    this.props.frontendSocket.joinMemoryChannel({
      handleEvent: this.handleMemoryReading,
    });
  }

  state: {
    queries: Query[],
    cloaks: Cloak[],
  };
  queryRemovalTime: number;
  handleQueryEvent: (queryEvent: QueryEvent) => void;
  handleRemoveQuery: (query: Query) => void;
  handleMemoryReading: (cloak: Cloak) => void;

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

  handleMemoryReading(cloak: Cloak) {
    const cloaks = _.chain([cloak, ...this.state.cloaks]).
      uniqBy((cloak) => cloak.id).
      sortBy((cloak) => cloak.name).
      value();
    this.setState({cloaks});
  }

  render() {
    return (
      <div>
        <CloaksView cloaks={this.state.cloaks} />
        <QueriesView queries={this.state.queries} />
      </div>
    );
  }
}

export default function renderACtivityMonitorView(data: Props, elem: Node) {
  const socket = new FrontendSocket(data.guardianToken);
  ReactDOM.render(<ActivityMonitorView frontendSocket={socket} {...data} />, elem);
}
