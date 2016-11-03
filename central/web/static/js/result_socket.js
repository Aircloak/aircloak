// @flow

import {Socket} from "phoenix";

type Callback = () => void;
type Callbacks = {joined?: Callback, failedJoin?: Callback, result?: Callback};

export class ResultSocket {
  constructor(userId: number, userToken: string) {
    this.userId = userId;
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();

    this.start = this.start.bind(this);
  }

  userId: number;
  socket: Socket;
  start: (callbacks: Callbacks) => void;

  start(callbacks: Callbacks) {
    const channel = this.socket.channel(`user:${this.userId}`, {});
    const noop = () => {};
    const {
      joined = noop,
      failedJoin = noop,
      result = noop,
    } = callbacks;

    channel.join()
      .receive("ok", joined)
      .receive("error", failedJoin);
    channel.on("result", result);
  }
}
