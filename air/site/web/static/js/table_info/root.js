import React from "react";
import ReactDOM from "react-dom";

const Columns = (props) =>
  <ul>
    {props.table.columns.map((column, i) =>
      <li key={i}>{column.name} - {column.type}</li>
    )}
  </ul>

const Table = (props) =>
  <li>
    <a onClick={props.onClick} href="#">
      {(() => {
        if (props.expanded) {
          return <span className="glyphicon glyphicon-minus"></span>;
        } else {
          return <span className="glyphicon glyphicon-plus"></span>;
        }
      })()}

      {props.table.id}
    </a>

    {(() => {
      if (props.expanded) {
        return <Columns table={props.table}/>;
      } else {
        return null;
      }
    })()}
  </li>;

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
      this.setState({expanded})
    }
  }

  expanded(table) {
    return this.state.expanded.has(table.id);
  }

  render() {
    return (<div>
      <h2>Tables</h2>

      <ul className="list-unstyled">
        {this.props.tables.map((table, i) =>
          <Table key={i} table={table} expanded={this.expanded(table)} onClick={this.toggleExpand(table)} />
        )}
      </ul>
    </div>);
  }
}

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
