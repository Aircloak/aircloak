// @flow

import React from "react";

import {Filter} from "./filter";

export type Column = {
  name: string,
  type: string,
  key_type: string,
};

const potentiallyRenderColumnIcon = (column: Column) => {
  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return <span className={`glyphicon glyphicon-${icon}`}>&nbsp;</span>;
  } else {
    return null;
  }
};

const columnClassName = (column: Column) => {
  if (column.key_type) {
    if (column.key_type === "user_id") {
      return "id-column";
    } else {
      return "key-column";
    }
  } else {
    return "name-column";
  }
};

export const ColumnsView = ({filter, columns}: {filter: Filter, columns: Column[]}) => {
  return (
    <table className="table table-condensed">
      <thead>
        <tr>
          <th>Column</th>
          <th>Type</th>
        </tr>
      </thead>

      <tbody>
        {filter.filterColumns(columns).map((column) => (
          <tr key={column.name}>
            {/* eslint-disable jsx-a11y/no-noninteractive-element-interactions,
                               jsx-a11y/click-events-have-key-events */}
            <td
              onClick={(event) => {
                event.preventDefault();
                window.insertWordInEditor(column.name);
              }}
              className={columnClassName(column)}
            >
              {potentiallyRenderColumnIcon(column)}
              {column.name}
            </td>
            {/* eslint-enable jsx-a11y/no-noninteractive-element-interactions,
                               jsx-a11y/click-events-have-key-events */}
            <td>{column.key_type ? `${column.key_type} (${column.type})` : column.type}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
};
