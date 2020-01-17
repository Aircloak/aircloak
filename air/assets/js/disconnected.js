// @flow

import React from "react";

import {Channel} from "phoenix";

type Props = {
  channel: Channel
}

type State = {
  isConnected: boolean
}

export class Disconnected extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {isConnected: true};

    this.componentWillUnmount = this.componentWillUnmount.bind(this);
    this.updateConnected = this.updateConnected.bind(this);

    this.connectedInterval = setInterval(this.updateConnected, 1000 /* 1 second */);
  }

  connectedInterval: IntervalID;

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
