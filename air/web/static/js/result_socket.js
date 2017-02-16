// @flow

import {Socket} from "phoenix";

type Callback = () => void;
type Callbacks = {joined?: Callback, failedJoin?: Callback, handleEvent?: Callback};

export class ResultSocket {
  constructor(sessionId: string, userToken: string) {
    this.sessionId = sessionId;
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();

    this.start = this.start.bind(this);
  }

  sessionId: string;
  socket: Socket;
  start: (callbacks: Callbacks) => void;

  start(callbacks: Callbacks) {
    const channel = this.socket.channel(`session:${this.sessionId}`, {});
    const noop = () => {};
    const {
      joined = noop,
      failedJoin = noop,
      handleEvent = noop,
    } = callbacks;

    channel.join()
      .receive("ok", joined)
      .receive("error", failedJoin);
    channel.on("result", handleEvent);
  }
}
