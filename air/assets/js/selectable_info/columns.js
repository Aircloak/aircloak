// @flow

import React from "react";

import { filterColumns, Higlighted } from "./filter";

export type Column = {
  name: string,
  type: string,
  key_type: string,
};

const potentiallyRenderColumnIcon = (column: Column) => {
  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return <span className={`fas fa-${icon}`}>&nbsp;</span>;
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

export const ColumnsView = ({
  filter,
  columns,
  table,
}: {
  table: string,
  filter: string,
  columns: Column[],
}) => {
  return (
    <table className="table table-condensed">
      <thead>
        <tr>
          <th>Column</th>
          <th>Type</th>
        </tr>
      </thead>

      <tbody>
        {filterColumns(table, columns, filter).map((item) => {
          return (
            <tr key={item.name}>
              {/* eslint-disable jsx-a11y/no-noninteractive-element-interactions,
                               jsx-a11y/click-events-have-key-events */}
              <td
                onClick={(event) => {
                  event.preventDefault();
                  window.insertWordInEditor(item.name);
                }}
                className={columnClassName(item)}
              >
                {potentiallyRenderColumnIcon(item)}
                <Higlighted table={table} column={item} field="name" />
              </td>
              {/* eslint-enable jsx-a11y/no-noninteractive-element-interactions,
                               jsx-a11y/click-events-have-key-events */}
              <td>
                {item.key_type ? (
                  <span>
                    <Higlighted table={table} column={item} field="key_type" />
                    <>(</>
                    <Higlighted table={table} column={item} field="type" />
                    <>)</>
                  </span>
                ) : (
                  <Higlighted table={table} column={item} field="type" />
                )}
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );
};
