// @flow

import React from "react";

import { filterColumns, Higlighted } from "./filter";
import ExplorerResultButton from "./explorer-result-button.js";
import type { NumberFormat } from "../number_format";

export type Column = {
  name: string,
  type: string,
  key_type: string,
  analysis?: any,
};

const typeColors = {
  boolean: "#fffbc6",
  integer: "#e8ffdd",
  real: "#e7f9cf",
  text: "#fff2f2",
  date: "#e8f2ff",
  datetime: "#e9e8ff",
};

const columnIcon = (column: Column) => {
  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return (
      <span
        className={`type-icon ${icon}-icon`}
        data-toggle="tooltip"
        style={{
          backgroundColor: typeColors[column.type] || "transparent",
        }}
        title={`${column.key_type}:${column.type}`}
      >
        <span className={`fas fa-${icon}`}>&nbsp;</span>
      </span>
    );
  } else {
    return (
      <span
        className="type-icon native-type"
        style={{
          backgroundColor: typeColors[column.type] || "transparent",
        }}
        data-toggle="tooltip"
        title={`${column.type}`}
      >
        {column.type[0].toUpperCase()}
      </span>
    );
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
  numberFormat,
}: {
  table: string,
  filter: string,
  columns: Column[],
  numberFormat: NumberFormat,
}) => {
  return (
    <ul className="list-group list-group-flush">
      {filterColumns(table, columns, filter).map((item) => {
        return (
          <li
            className={`list-group-item p-1  pr-3 ${columnClassName(
              item
            )} d-flex justify-content-between align-items-center`}
            key={item.name}
          >
            <button
              className="btn"
              onClick={(event) => {
                event.preventDefault();
                window.insertWordInEditor(item.name);
              }}
            >
              {columnIcon(item)}
              <Higlighted table={table} column={item} field="name" />{" "}
              {item.key_type && (
                <>
                  {" ("}
                  <Higlighted table={table} column={item} field="key_type" />
                  {")"}
                </>
              )}
            </button>
            {item.analysis && (
              <ExplorerResultButton item={item} numberFormat={numberFormat} />
            )}
          </li>
        );
      })}
    </ul>
  );
};
