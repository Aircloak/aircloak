// @flow

import React from "react";
import _ from "lodash";
import {Channel} from "phoenix";

import {QueriesView} from "./queries";
import type {Query} from "./query";
import {CloaksView} from "./cloaks";
import type {Cloak} from "./cloak";

import {FrontendSocket} from "../frontend_socket";
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
  frontendSocket: FrontendSocket,
  queries: Query[],
  cloaks: Cloak[],
};

export default class ActivityMonitorView extends React.Component {
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

    this.channel = this.props.frontendSocket.joinAllQueryEventsChannel({
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
  channel: Channel;

  handleQueryEvent: (queryEvent: QueryEvent) => void;
  handleRemoveQuery: (queryId: string) => void;
  handleMemoryReading: (cloak: Cloak) => void;

  handleRemoveQuery(queryId: string) {
    const queries = _.reject(this.state.queries, (query) => query.id === queryId);
    this.setState({queries});
  }

  conditionallyScheduleQueryRemoval(queryEvent: QueryEvent) {
    if (isFinished(queryEvent.event)) {
      setTimeout(() => this.handleRemoveQuery(queryEvent.query_id), this.queryRemovalTime);
    }
  }

  handleQueryEvent(queryEvent: QueryEvent) {
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

  handleMemoryReading(newOrUpdatedCloak: Cloak) {
    const cloaks = _.chain([newOrUpdatedCloak, ...this.state.cloaks]).
      uniqBy((cloak) => cloak.id).
      sortBy((cloak) => cloak.name).
      value();
    this.setState({cloaks});
  }

  render() {
    return (
      <div>
        <Disconnected channel={this.channel} />
        <CloaksView cloaks={this.state.cloaks} />
        <QueriesView queries={this.state.queries} />
      </div>
    );
  }
}
