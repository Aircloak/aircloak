// @flow

import React from "react";

import ImmutableSingleQuery from "./immutable_single_query";
import type {Result} from "./result";
import FrontendSocket from "../frontend_socket";
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
    const {frontendSocket, result} = this.props;

    frontendSocket.joinUpdatesForQuery(result.id, {
      handleEvent: this.resultReceived,
    });
  }

  // eslint-disable-next-line react/static-property-placement
  static contextType = AuthContext;

  resultReceived = (result: Result) => {
    this.setState({result});
  }

  render = () => {
    const {numberFormat, debugModeEnabled} = this.props;
    const {result} = this.state;
    const {authentication} = this.context;
    return (
      <ImmutableSingleQuery
        numberFormat={numberFormat}
        debugModeEnabled={debugModeEnabled}
        result={result}
        authentication={authentication}
      />
    );
  }
}
