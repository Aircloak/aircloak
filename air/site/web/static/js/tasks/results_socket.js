import {Socket} from "phoenix";

export class ResultSocket {
  constructor(taskId, userToken) {
    this.taskId = taskId;
    this.socket = new Socket("/frontend/socket", {params: {token: userToken}});
    this.socket.connect();

    this.start = this.start.bind(this);
  }

  start(callbacks) {
    const channel = this.socket.channel(`task:${this.taskId}`, {});
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
