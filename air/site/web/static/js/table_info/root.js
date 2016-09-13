import React from "react";
import ReactDOM from "react-dom";

const TableInfo = (props) => <div>
  <h2>Tables</h2>

  <ul className="list-unstyled">
  </ul>
</div>

export default function renderTableInfo(data, elem) {
  ReactDOM.render(<TableInfo {...data} />, elem);
}
