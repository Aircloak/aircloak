// @flow

import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import _ from "lodash";

import {QueriesView} from "./queries";
import type {Query} from "./query";

import {QuerySocket} from "../query_socket";

type Props = {
  userId: number,
  guardianToken: string,
  CSRFToken: string,
  querySocket: QuerySocket,
};

class ActivityMonitorView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {
      queries: [],
    };

    this.handleQueryEvent = this.handleQueryEvent.bind(this);

    this.props.querySocket.joinAllQueryEventsChannel({
      handleEvent: this.handleQueryEvent,
    });
  }

  state: {
    queries: Query[],
  };
  handleQueryEvent: () => void;

  handleQueryEvent(queryEvent) {
    var queries = this.state.queries;

    if (queryEvent.event == "started") {
      const query = {
        id: queryEvent.query_id,
        analyst_name: queryEvent.query.user.name,
        data_source_name: queryEvent.query.data_source.name,
        state: queryEvent.event,
      }
      queries.unshift(query);

    } else {
      queries = _.map(queries, (existingQuery) => {
        if (existingQuery.id == queryEvent.query_id) {
          existingQuery.state = queryEvent.event;
        }
        return existingQuery;
      });
    }

    this.setState({queries});
  }

  render() {
    return (
      <div>
        <QueriesView queries={this.state.queries} />
      </div>
    );
  }
}

export default function renderACtivityMonitorView(data: Props, elem: Node) {
  const socket = new QuerySocket(data.guardianToken);
  ReactDOM.render(<ActivityMonitorView querySocket={socket} {...data} />, elem);
}
