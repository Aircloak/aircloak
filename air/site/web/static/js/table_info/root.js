import React from "react";
import ReactDOM from "react-dom"

import {Table} from "./table";

class TableInfo extends React.Component {
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

  render() {
    return (<div className="panel panel-default">
      <div className="panel-heading"><strong>Tables</strong></div>

      <div className="list-group">
        {this.props.tables.map((table, i) =>
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
