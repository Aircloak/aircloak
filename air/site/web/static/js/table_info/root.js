import React from "react";
import ReactDOM from "react-dom";

const Columns = (props) =>
  <table className="table">
    <tr>
      <th>Column</th>
      <th>Type</th>
    </tr>
    {props.columns.map((column, i) =>
      <tr key={i}>
        <td>{column.name}</td>
        <td>{column.type}</td>
      </tr>
    )}
  </table>;

const Table = (props) =>
  <a href="#" onClick={props.onClick} className="list-group-item">
    <div className="list-group-item-heading">
      {(() => {
        if (props.expanded) {
          return <span className="glyphicon glyphicon-minus"></span>;
        } else {
          return <span className="glyphicon glyphicon-plus"></span>;
        }
      })()}

      &nbsp;

      {props.table.id}
    </div>

    {(() => {
      if (props.expanded) {
        return <Columns columns={props.table.columns} />;
      } else {
        return null;
      }
    })()}
  </a>;

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

Columns.propTypes = {
  columns: React.PropTypes.arrayOf(React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
  })).isRequired,
};

Table.propTypes = {
  table: React.PropTypes.shape({
    id: React.PropTypes.string.isRequired,
    columns: Columns.propTypes.columns,
  }),
};

TableInfo.propTypes = {
  tables: React.PropTypes.arrayOf(Table.propTypes.table).isRequired,
};

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
