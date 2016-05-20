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
    channel.join()
      .receive("ok", resp => callbacks.joined(resp))
      .receive("error", resp => callbacks.failed_join(resp));
    channel.on("result", payload => callbacks.result(payload));
  }
}
