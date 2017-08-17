// @flow

import React from "react";

import {Results} from "./results";
import {PropertiesView} from "./properties";
import type {Result} from "./result";
import {FrontendSocket} from "../frontend_socket";
import type {NumberFormat} from "../number_format";

type Props = {
  result: Result,
  frontendSocket: FrontendSocket,
  numberFormat: NumberFormat,
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
      <Results numberFormat={this.props.numberFormat} results={[this.state.result]} />
    </div>);
  }
}

QueryView.contextTypes = {
  authentication: React.PropTypes.object.isRequired,
};
