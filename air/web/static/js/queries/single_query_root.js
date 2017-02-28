// @flow

import React from "react";
import ReactDOM from "react-dom";

import {Results} from "./results";
import type {Result} from "./result";
import {QuerySocket} from "../query_socket";

type Props = {
  result: Result,
  guardianToken: string,
  querySocket: QuerySocket,
};

class QueryView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {
      result: props.result,
    };

    this.resultReceived = this.resultReceived.bind(this);

    this.props.querySocket.joinUpdatesForQuery(props.result.id, {
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
      <Results results={[this.state.result]} />
    </div>);
  }
}

export default function renderQueryView(data: Props, elem: Node) {
  const socket = new QuerySocket(data.guardianToken);
  ReactDOM.render(<QueryView querySocket={socket} {...data} />, elem);
}
