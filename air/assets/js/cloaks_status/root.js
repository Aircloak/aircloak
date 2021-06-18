// @flow

import type { Element } from "React";
import React from "react";
import sortBy from "lodash/sortBy";
import type { Channel } from "phoenix";

import CloaksStatsView from "./cloaks_stats";
import type { CloakStat } from "./cloak_stats";

import FrontendSocket from "../frontend_socket";
import Disconnected from "../disconnected";

type Props = {
  frontendSocket: FrontendSocket,
  cloakStats: CloakStat[],
};

type State = {
  cloakStats: CloakStat[],
};

export default class CloaksStatusView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    const { cloakStats, frontendSocket } = this.props;

    this.state = {
      cloakStats,
    };

    this.handleCloakStatsUpdate = this.handleCloakStatsUpdate.bind(this);

    this.channel = frontendSocket.joinCloakStatsChannel({
      handleEvent: this.handleCloakStatsUpdate,
    });
  }

  channel: Channel;

  handleCloakStatsUpdate:
    | any
    | ((cloakStatsUpdate: {
        cloakStats: Array<CloakStat>,
        ...
      }) => void) = (cloakStatsUpdate: { cloakStats: CloakStat[] }) => {
    const cloakStats = sortBy(cloakStatsUpdate.cloakStats, "name");
    this.setState({ cloakStats });
  };

  render: () => Element<"div"> = () => {
    const { cloakStats } = this.state;
    return (
      <div>
        <Disconnected channel={this.channel} />
        <CloaksStatsView cloakStats={cloakStats} />
      </div>
    );
  };
}
