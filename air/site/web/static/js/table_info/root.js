import React from "react";
import ReactDOM from "react-dom";

const Table = (props) =>
  <li>
    <a href="#">
      <span className="glyphicon glyphicon-plus"></span>
      {props.name}
    </a>
  </li>;

const TableInfo = (props) =>
  <div>
    <h2>Tables</h2>

    <ul className="list-unstyled">
      {props.tableNames.map((name, i) => <Table key={i} name={name} />)}
    </ul>
  </div>;

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
