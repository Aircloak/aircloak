import React from "react";
import ReactDOM from "react-dom";
import _ from "lodash";

import {Table} from "./table";

class TableInfo extends React.Component {
  constructor(props) {
    super(props);

    this.state = {expanded: new Set()};

    this.toggleExpand = this.toggleExpand.bind(this);
  }

  toggleExpand(table) {
    return (event) => {
      event.preventDefault();

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
    return _.sortBy(this.props.tables, (table) => table.id.toLowerCase());
  }

  render() {
    return (<div className="panel panel-default table-info">
      <div className="panel-heading"><strong>Tables</strong></div>

      <div className="list-group">
        {this.tables().map((table, i) =>
          <Table key={i} table={table} expanded={this.expanded(table)} onClick={this.toggleExpand(table)} />
        )}
      </div>
    </div>);
  }
}

TableInfo.propTypes = {
  tables: React.PropTypes.arrayOf(Table.propTypes.table).isRequired,
};

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
