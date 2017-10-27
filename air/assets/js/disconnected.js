// @flow

import React from "react";

import {Channel} from "phoenix";

export class Disconnected extends React.Component {
  constructor(props: {channel: Channel}) {
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
    this.setState({isConnected: this.props.channel.isJoined()});
  }

  render() {
    if (!this.state.isConnected) {
      return (<p className="alert alert-warning">
        Connection to Aircloak lost. The system might be down or you might have lost your network connection.
      </p>);
    } else {
      return null;
    }
  }
}
