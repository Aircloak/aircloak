// @flow

import React from "react";

import {Results} from "./results";
import {PropertiesView} from "./properties";
import type {Result} from "./result";
import {FrontendSocket} from "../frontend_socket";

type Props = {
  result: Result,
  frontendSocket: FrontendSocket,
  CSRFToken: string,
};

export default class QueryView extends React.Component {
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

  state: {
    result: Result,
  }
  resultReceived: (result: Result) => void;

  resultReceived(result: Result) {
    this.setState({result});
  }

  render() {
    return (<div>
      <h3>Properties</h3>
      <PropertiesView {...this.state.result} />

      <h3>Query</h3>
      <Results results={[this.state.result]} CSRFToken={this.props.CSRFToken} />
    </div>);
  }
}
