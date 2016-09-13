import React from "react";
import ReactDOM from "react-dom";

const Columns = (props) =>
  <ul>
    <li>Column 1</li>
    <li>Column 2</li>
    <li>Column 3</li>
    <li>Column 4</li>
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

      {props.name}
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
        expanded.delete(table);
      } else {
        expanded.add(table);
      }
      this.setState({expanded})
    }
  }

  expanded(table) {
    return this.state.expanded.has(table);
  }

  render() {
    return (<div>
      <h2>Tables</h2>

      <ul className="list-unstyled">
        {this.props.tableNames.map((name, i) =>
          <Table key={i} name={name} expanded={this.expanded(name)} onClick={this.toggleExpand(name)} />
        )}
      </ul>
    </div>);
  }
}

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
