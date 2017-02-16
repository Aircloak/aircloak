// @flow

import {Socket} from "phoenix";

type Callback = () => void;
type Callbacks = {joined?: Callback, failedJoin?: Callback, handleEvent?: Callback};

export class QuerySocket {
  constructor(userToken: string) {
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();

    this.joinSessionChannel = this.joinSessionChannel.bind(this);
  }

  socket: Socket;
  joinSessionChannel: (sessionId: string, callbacks: Callbacks) => void;

  joinSessionChannel(sessionId: string, callbacks: Callbacks) {
    this.joinChannel(callbacks, `session:${sessionId}`, "result");
  }

  joinChannel(callbacks: Callbacks, channelName: string, eventName: string) {
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
    channel.on(eventName, handleEvent);
  }
}
