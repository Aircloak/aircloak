// @flow

import type { Element } from "React";
import React from "react";
import type { Channel } from "phoenix";

import QueriesView from "./queries";
import type { Query } from "./query";

import FrontendSocket from "../frontend_socket";
import { isFinished } from "../queries/state";
import Disconnected from "../disconnected";

type QueryEvent = {
  query_id: string,
  event: string,
  query: Query,
};

type Props = {
  frontendSocket: FrontendSocket,
  queries: Query[],
};

type State = {
  queries: Query[],
};

export default class ActiveQueriesView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    const { queries, frontendSocket } = this.props;

    // How long to display a query after it has completed
    this.queryRemovalTime = 10000; // 10 seconds

    this.state = {
      queries,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);
    this.handleRemoveQuery = this.handleRemoveQuery.bind(this);

    this.channel = frontendSocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
  }

  channel: Channel;

  queryRemovalTime: number;

  handleRemoveQuery: any | ((queryId: string) => void) = (queryId: string) => {
    this.setState((state) => ({
      queries: state.queries.filter((query) => query.id !== queryId),
    }));
  };

  conditionallyScheduleQueryRemoval: (queryEvent: QueryEvent) => void = (
    queryEvent: QueryEvent
  ) => {
    if (isFinished(queryEvent.event)) {
      setTimeout(
        () => this.handleRemoveQuery(queryEvent.query_id),
        this.queryRemovalTime
      );
    }
  };

  handleQueryEvent: any | ((queryEvent: QueryEvent) => void) = (
    queryEvent: QueryEvent
  ) => {
    this.conditionallyScheduleQueryRemoval(queryEvent);

    if (queryEvent.event === "started") {
      const newQuery = queryEvent.query;
      this.setState((state) => ({ queries: [newQuery, ...state.queries] }));
    } else {
      this.setState((state) => ({
        queries: state.queries.map((existingQuery) => {
          if (existingQuery.id === queryEvent.query_id) {
            const alteredQuery = { ...existingQuery };
            alteredQuery.state = queryEvent.event;
            return alteredQuery;
          } else {
            return existingQuery;
          }
        }),
      }));
    }
  };

  render: () => Element<"div"> = () => {
    const { queries } = this.state;
    return (
      <div>
        <Disconnected channel={this.channel} />
        <QueriesView queries={queries} />
      </div>
    );
  };
}
