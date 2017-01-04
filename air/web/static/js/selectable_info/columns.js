// @flow

import React from "react";

export type Column = {
  name: string,
  type: string,
  user_id: boolean,
};

export const ColumnsView = (props: {columns: Column[]}) =>
  <table className="table table-condensed">
    <thead>
      <tr>
        <th>Column</th>
        <th>Type</th>
      </tr>
    </thead>

    <tbody>
      {props.columns.map((column, i) =>
        <tr key={i}>
          <td
            onClick={(event) => {
              event.preventDefault();
              window.insertWordInEditor(column.name);
            }}
            className={column.user_id ? "id-column" : "name-column"}
          >
            {column.name}
          </td>
          <td>{column.type}</td>
        </tr>
      )}
    </tbody>
  </table>;
