import React from "react";
import _ from "lodash";

export const Columns = (props) => {
  return (<table className="table table-condensed">
    <thead>
      <tr>
        <th>Column</th>
        <th>Type</th>
      </tr>
    </thead>

    <tbody>
      {props.columns.map((column, i) =>
        <tr key={i}>
          <td className={column.user_id?"id-column":"name-column"}>{column.name}</td>
          <td>{column.type}</td>
        </tr>
      )}
    </tbody>
  </table>);
};

Columns.propTypes = {
  columns: React.PropTypes.arrayOf(React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
    user_id: React.PropTypes.bool,
  })).isRequired,
};
