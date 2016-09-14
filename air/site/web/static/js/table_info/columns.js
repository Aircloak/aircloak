import React from "react";

export const Columns = (props) =>
  <table className="table">
    <thead>
      <tr>
        <th>Column</th>
        <th>Type</th>
      </tr>
    </thead>

    <tbody>
      {props.columns.map((column, i) =>
        <tr key={i}>
          <td>{column.name}</td>
          <td>{column.type}</td>
        </tr>
      )}
    </tbody>
  </table>;

Columns.propTypes = {
  columns: React.PropTypes.arrayOf(React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
  })).isRequired,
};
