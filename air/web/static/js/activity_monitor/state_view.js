// @flow

import React from "react";

export type State = {
  state: string,
};

export class StateView extends React.Component {
  constructor(props: State) {
    super(props);
  }

  props: State;

  render() {
    var labelType = "info";
    if (this.props.state == "completed") {
      labelType = "success";
    }
    return (<span className={'label label-' + labelType}>{this.props.state}</span>);
  }
}

