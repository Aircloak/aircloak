// @flow

import React from "react";
import ReactDOM from "react-dom";

import {Results} from "./results";
import {PropertiesView} from "./properties";
import type {Result} from "./result";
import {FrontendSocket} from "../frontend_socket";

type Props = {
  result: Result,
  frontendSocket: FrontendSocket,
};

type AppProperties = {
  guardianToken: string,
} & Props

class QueryView extends React.Component {
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
      <Results results={[this.state.result]} />
    </div>);
  }
}

export default function renderQueryView(data: AppProperties, elem: Node) {
  const socket = new FrontendSocket(data.guardianToken);
  ReactDOM.render(<QueryView frontendSocket={socket} {...data} />, elem);
}
