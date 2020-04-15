// @flow

import React from "react";
import sortBy from "lodash/sortBy";
import { Channel } from "phoenix";

import QueriesView from "./queries";
import type { Query } from "./query";
import CloaksStatsView from "./cloaks_stats";
import type { CloakStat } from "./cloak_stats";

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
  cloakStats: CloakStat[],
};

type State = {
  queries: Query[],
  cloakStats: CloakStat[],
};

export default class ActivityMonitorView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    const { queries, cloakStats, frontendSocket } = this.props;

    // How long to display a query after it has completed
    this.queryRemovalTime = 10000; // 10 seconds

    this.state = {
      queries,
      cloakStats,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);
    this.handleRemoveQuery = this.handleRemoveQuery.bind(this);
    this.handleCloakStatsUpdate = this.handleCloakStatsUpdate.bind(this);

    this.channel = frontendSocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
    frontendSocket.joinCloakStatsChannel({
      handleEvent: this.handleCloakStatsUpdate,
    });
  }

  channel: Channel;

  queryRemovalTime: number;

  handleRemoveQuery = (queryId: string) => {
    this.setState((state) => ({
      queries: state.queries.filter((query) => query.id !== queryId),
    }));
  };

  conditionallyScheduleQueryRemoval = (queryEvent: QueryEvent) => {
    if (isFinished(queryEvent.event)) {
      setTimeout(
        () => this.handleRemoveQuery(queryEvent.query_id),
        this.queryRemovalTime
      );
    }
  };

  handleQueryEvent = (queryEvent: QueryEvent) => {
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

  handleCloakStatsUpdate = (cloakStatsUpdate: { cloakStats: CloakStat[] }) => {
    const cloakStats = sortBy(cloakStatsUpdate.cloakStats, "name");
    this.setState({ cloakStats });
  };

  render = () => {
    const { cloakStats, queries } = this.state;
    return (
      <div>
        <Disconnected channel={this.channel} />
        <CloaksStatsView cloakStats={cloakStats} />
        <QueriesView queries={queries} />
      </div>
    );
  };
}
