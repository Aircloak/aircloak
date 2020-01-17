// @flow

import React from "react";

import ImmutableSingleQuery from "./immutable_single_query";
import type {Result} from "./result";
import {FrontendSocket} from "../frontend_socket";
import type {NumberFormat} from "../number_format";
import {AuthContext} from "../authentication_provider";

type Props = {
  result: Result,
  frontendSocket: FrontendSocket,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
};

type State = {
  result: Result
}

export default class QueryView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {
      result: props.result,
    };

    this.resultReceived = this.resultReceived.bind(this);

    this.props.frontendSocket.joinUpdatesForQuery(props.result.id, {
      handleEvent: this.resultReceived,
    });
  }

  static contextType = AuthContext;

  resultReceived = (result: Result) => {
    this.setState({result});
  }

  render = () => {
    return (<ImmutableSingleQuery
      numberFormat={this.props.numberFormat}
      debugModeEnabled={this.props.debugModeEnabled}
      result={this.state.result}
      authentication={this.context.authentication}
    />);
  }
}
