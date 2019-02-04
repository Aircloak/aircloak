// @flow

import React from "react";

import {Filter} from "./filter";

export type Column = {
  name: string,
  type: string,
  user_id: boolean,
};

const potentiallyRenderColumnIcon = (column: Column) => {
  if (column.user_id) {
    return <span className="glyphicon glyphicon-user" >&nbsp;</span>;
  } else {
    return null;
  }
};


export const ColumnsView = (props: {filter: Filter, columns: Column[]}) =>
  <table className="table table-condensed">
    <thead>
      <tr>
        <th>Column</th>
        <th>Type</th>
      </tr>
    </thead>

    <tbody>
      {props.filter.filterColumns(props.columns).map((column, i) =>
        <tr key={i}>
          <td
            onClick={(event) => {
              event.preventDefault();
              window.insertWordInEditor(column.name);
            }}
            className={column.user_id ? "id-column" : "name-column"}
          >
            {potentiallyRenderColumnIcon(column)}
            {column.name}
          </td>
          <td>{column.type}</td>
        </tr>
      )}
    </tbody>
  </table>;
