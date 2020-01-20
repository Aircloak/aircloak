// @flow

import React from "react";
import _ from "lodash";
import {Channel} from "phoenix";

import QueriesView from "./queries";
import type {Query} from "./query";
import CloaksStatsView from "./cloaks_stats";
import type {CloakStat} from "./cloak_stats";

import FrontendSocket from "../frontend_socket";
import {isFinished} from "../queries/state";
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
    const {queries, cloakStats, frontendSocket} = this.props;

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
    const {queries} = this.state;
    const filteredQueries = _.reject(queries, (query) => query.id === queryId);
    this.setState({queries: filteredQueries});
  }

  conditionallyScheduleQueryRemoval = (queryEvent: QueryEvent) => {
    if (isFinished(queryEvent.event)) {
      setTimeout(() => this.handleRemoveQuery(queryEvent.query_id), this.queryRemovalTime);
    }
  }

  handleQueryEvent = (queryEvent: QueryEvent) => {
    this.conditionallyScheduleQueryRemoval(queryEvent);
    const {queries} = this.state;

    if (queryEvent.event === "started") {
      const newQuery = queryEvent.query;
      this.setState({queries: [newQuery, ...queries]});
    } else {
      this.setState({
        queries: _.map(queries, (existingQuery) => {
          if (existingQuery.id === queryEvent.query_id) {
            const alteredQuery = _.clone(existingQuery);
            alteredQuery.state = queryEvent.event;
            return alteredQuery;
          } else {
            return existingQuery;
          }
        }),
      });
    }
  }

  handleCloakStatsUpdate = (cloakStatsUpdate: {cloakStats: CloakStat[]}) => {
    const cloakStats = _.sortBy(cloakStatsUpdate.cloakStats, ["name"]);
    this.setState({cloakStats});
  }

  render = () => {
    const {cloakStats, queries} = this.state;
    return (
      <div>
        <Disconnected channel={this.channel} />
        <CloaksStatsView cloakStats={cloakStats} />
        <QueriesView queries={queries} />
      </div>
    );
  }
}
