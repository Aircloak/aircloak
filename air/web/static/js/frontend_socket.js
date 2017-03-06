// @flow

import {Socket} from "phoenix";

type Callback = (event: any) => void;
type Callbacks = {joined?: Callback, failedJoin?: Callback, handleEvent?: Callback};

export class FrontendSocket {
  constructor(userToken: string) {
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();
  }

  socket: Socket;

  isConnected() { return this.socket.isConnected(); }

  joinUserQueriesChannel(userId: number, callbacks: Callbacks) {
    return this.joinChannel(callbacks, `user_queries:${userId}`, ["result", "state_change"]);
  }

  joinUpdatesForQuery(queryId: string, callbacks: Callbacks) {
    return this.joinChannel(callbacks, `query:${queryId}`, ["result", "state_change"]);
  }

  joinAllQueryEventsChannel(callbacks: Callbacks) {
    return this.joinChannel(callbacks, "state_changes:all", ["state_change"]);
  }

  joinMemoryChannel(callbacks: Callbacks) {
    return this.joinChannel(callbacks, "memory_readings", ["new_reading"]);
  }

  joinChannel(callbacks: Callbacks, channelName: string, eventNames: string[]) {
    const channel = this.socket.channel(channelName, {});
    const noop = () => {};
    const {
      joined = noop,
      failedJoin = noop,
      handleEvent = noop,
    } = callbacks;

    channel.join()
      .receive("ok", joined)
      .receive("error", failedJoin);
    eventNames.forEach((name) => { channel.on(name, handleEvent); });

    return channel;
  }
}
