// @flow

import React from "react";
import ReactDOM from "react-dom";
import _ from "lodash";

import {TableView} from "./table";
import type {Table} from "./table";

type Props = {tables: Table[], views: Table[]};

class TableInfo extends React.Component {
  props: Props;
  state: {expanded: Set<string>};
  toggleExpand: (t: Table) => (() => void);

  constructor(props) {
    super(props);

    this.state = {expanded: new Set()};

    this.toggleExpand = this.toggleExpand.bind(this);
  }

  toggleExpand(table) {
    return () => {
      const expanded = this.state.expanded;
      if (this.expanded(table)) {
        expanded.delete(table.id);
      } else {
        expanded.add(table.id);
      }
      this.setState({expanded});
    };
  }

  expanded(table) {
    return this.state.expanded.has(table.id);
  }

  tables() {
    return (
      _
      .chain(this.props.views)
      .map((view) => ({
        id: view.name,
        columns: view.result_info.columns,
        editLink: view.edit_link,
        deleteHtml: view.delete_html,
      }))
      .concat(this.props.tables)
      .sortBy((table) => table.id.toLowerCase())
      .value()
    );
  }

  render() {
    return (
      <div className="panel panel-default table-info">
        <div className="panel-heading"><strong>Tables</strong></div>

        <div className="table-info-content">
          {this.tables().map((table, i) =>
            <
              TableView
              key={i}
              table={table}
              expanded={this.expanded(table)}
              onClick={this.toggleExpand(table)}
            />
          )}
        </div>
      </div>
    );
  }
}

export default function renderTableInfo(data: Props, elem: Node) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
