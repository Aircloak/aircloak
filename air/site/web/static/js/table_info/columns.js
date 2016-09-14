import React from "react";

export const Columns = (props) =>
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

Columns.propTypes = {
  columns: React.PropTypes.arrayOf(React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
  })).isRequired,
};
