// @flow

import { Socket, LongPoll } from "phoenix";
import type { Channel } from "phoenix";

type Callback = (event: any) => void;
type Callbacks = {
  joined?: Callback,
  failedJoin?: Callback,
  handleEvent?: Callback,
};

export default class FrontendSocket {
  constructor(transportName: string, userToken: string) {
    this.socket = new Socket("/frontend/socket", {
      params: { token: userToken },
      transport: FrontendSocket.transport(transportName),
    });

    this.socket.connect();
  }

  static transport(transportName: string): any {
    switch (transportName) {
      case "websocket":
        return WebSocket;
      case "long_polling":
        return LongPoll;
      default:
        return WebSocket;
    }
  }

  socket: Socket;

  isConnected(): any {
    return this.socket.isConnected();
  }

  joinUserQueriesChannel(userId: number, callbacks: Callbacks): any {
    return this.joinChannel(callbacks, `user_queries:${userId}`, [
      "state_change",
    ]);
  }

  joinUpdatesForQuery(queryId: string, callbacks: Callbacks): any {
    return this.joinChannel(callbacks, `query:${queryId}`, ["state_change"]);
  }

  joinAllQueryEventsChannel(callbacks: Callbacks): any {
    return this.joinChannel(callbacks, "state_changes:all", ["state_change"]);
  }

  joinDataSourceChannel(dataSourceName: string, callbacks: Callbacks): any {
    return this.joinChannel(callbacks, `data_source:${dataSourceName}`, [
      "status",
    ]);
  }

  joinCloakStatsChannel(callbacks: Callbacks): any {
    return this.joinChannel(callbacks, "cloak_stats", ["updated_cloak_infos"]);
  }

  joinTypeCheckChannel(userId: number, callbacks: Callbacks): any {
    return this.joinChannel(callbacks, `type_check:${userId}`, [
      "state_change",
    ]);
  }

  joinSelectablesChannel(
    dataSourceName: string,
    userId: number,
    callbacks: Callbacks
  ): any {
    return this.joinChannel(
      callbacks,
      `selectables:${dataSourceName}:${userId}`,
      ["selectables_change"]
    );
  }

  joinChannel(
    callbacks: Callbacks,
    channelName: string,
    eventNames: string[]
  ): Channel {
    const channel = this.socket.channel(channelName, {});
    const noop = () => {};
    const { joined = noop, failedJoin = noop, handleEvent = noop } = callbacks;

    channel.join().receive("ok", joined).receive("error", failedJoin);
    eventNames.forEach((name) => {
      channel.on(name, handleEvent);
    });

    return channel;
  }
}
