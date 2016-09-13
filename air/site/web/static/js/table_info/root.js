import React from "react";
import ReactDOM from "react-dom";

const Columns = (props) =>
  <ul>
    {props.columns.map((column, i) =>
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
        return <Columns columns={props.table.columns}/>;
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

Columns.propTypes = {
  columns: React.PropTypes.arrayOf(React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
  })).isRequired,
}

Table.propTypes = {
  table: React.PropTypes.shape({
    id: React.PropTypes.string.isRequired,
    columns: Columns.propTypes.columns,
  })
}

TableInfo.propTypes = {
  tables: React.PropTypes.arrayOf(Table.propTypes.table).isRequired,
}

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
