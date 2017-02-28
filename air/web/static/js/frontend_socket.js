// @flow

import {Socket} from "phoenix";

type Callback = (event: any) => void;
type Callbacks = {joined?: Callback, failedJoin?: Callback, handleEvent?: Callback};

export class FrontendSocket {
  constructor(userToken: string) {
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();

    this.joinSessionChannel = this.joinSessionChannel.bind(this);
    this.joinAllQueryEventsChannel = this.joinAllQueryEventsChannel.bind(this);
    this.joinMemoryChannel = this.joinMemoryChannel.bind(this);
  }

  socket: Socket;
  joinSessionChannel: (sessionId: string, callbacks: Callbacks) => void;
  joinAllQueryEventsChannel: (callbacks: Callbacks) => void;
  joinMemoryChannel: (callbacks: Callbacks) => void;

  isConnected() { return this.socket.isConnected(); }

  joinSessionChannel(sessionId: string, callbacks: Callbacks) {
    return this.joinChannel(callbacks, `session:${sessionId}`, ["result", "state_change"]);
  }

  joinAllQueryEventsChannel(callbacks: Callbacks) {
    return this.joinChannel(callbacks, "state_changes:all", ["state_change"]);
  }

  joinMemoryChannel(callbacks: Callbacks) {
    this.joinChannel(callbacks, "memory_readings", ["new_reading"]);
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
