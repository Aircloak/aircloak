// @flow

import React from "react";
import _ from "lodash";
import {Channel} from "phoenix";

import {QueriesView} from "./queries";
import type {Query} from "./query";
import {CloaksStatsView} from "./cloaks_stats";
import type {CloakStat} from "./cloak_stats";

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
  socketToken: string,
  frontendSocket: FrontendSocket,
  queries: Query[],
  cloak_stats: CloakStat[],
};

type State = {
  queries: Query[],
  cloakStats: CloakStat[],
};

export default class ActivityMonitorView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    // How long to display a query after it has completed
    this.queryRemovalTime = 10000; // 10 seconds

    this.state = {
      queries: this.props.queries,
      cloakStats: this.props.cloak_stats,
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);
    this.handleRemoveQuery = this.handleRemoveQuery.bind(this);
    this.handleCloakStatsUpdate = this.handleCloakStatsUpdate.bind(this);

    this.channel = this.props.frontendSocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
    this.props.frontendSocket.joinCloakStatsChannel({
      handleEvent: this.handleCloakStatsUpdate,
    });
  }

  channel: Channel;

  queryRemovalTime: number;

  handleRemoveQuery = (queryId: string) => {
    const queries = _.reject(this.state.queries, (query) => query.id === queryId);
    this.setState({queries});
  }

  conditionallyScheduleQueryRemoval = (queryEvent: QueryEvent) => {
    if (isFinished(queryEvent.event)) {
      setTimeout(() => this.handleRemoveQuery(queryEvent.query_id), this.queryRemovalTime);
    }
  }

  handleQueryEvent = (queryEvent: QueryEvent) => {
    this.conditionallyScheduleQueryRemoval(queryEvent);

    if (queryEvent.event === "started") {
      const newQuery = queryEvent.query;
      this.setState({queries: [newQuery, ...this.state.queries]});
    } else {
      this.setState({
        queries: _.map(this.state.queries, (existingQuery) => {
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

  handleCloakStatsUpdate = (cloakStatsUpdate: {cloak_stats: CloakStat[]}) => {
    const cloakStats = _.sortBy(cloakStatsUpdate.cloak_stats, ["name"]);
    this.setState({cloakStats});
  }

  render = () => (
    <div>
      <Disconnected channel={this.channel} />
      <CloaksStatsView cloakStats={this.state.cloakStats} />
      <QueriesView queries={this.state.queries} />
    </div>
  )
}
