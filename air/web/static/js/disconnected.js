// @flow

import React from "react";

import {QuerySocket} from "./query_socket";

export class Disconnected extends React.Component {
  constructor(props: {socket: QuerySocket}) {
    super(props);

    this.state = {isConnected: true};

    this.componentWillUnmount = this.componentWillUnmount.bind(this);
    this.updateConnected = this.updateConnected.bind(this);

    this.connectedInterval = setInterval(this.updateConnected, 1000 /* 1 second */);
  }

  state: {
    isConnected: boolean,
  }
  connectedInterval: number;

  componentWillUnmount: () => void;
  updateConnected: () => void;

  componentWillUnmount() {
    clearInterval(this.connectedInterval);
  }

  updateConnected() {
    this.setState({isConnected: this.props.socket.isConnected()});
  }

  render() {
    if (!this.state.isConnected) {
      return <p className="alert alert-warning">Connection lost</p>;
    } else {
      return null;
    }
  }
}
