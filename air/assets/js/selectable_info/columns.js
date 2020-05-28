// @flow

import React from "react";

import { filterColumns, Higlighted } from "./filter";
import SparklineOverview from "./sparkline_overview";

export type Column = {
  name: string,
  type: string,
  key_type: string,
};

const potentiallyRenderColumnIcon = (column: Column) => {
  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return (
      <span
        style={{
          border: "1px solid grey",
          borderRadius: "2px",
          padding: "3px",
          paddingRight: "0",
          marginRight: ".6em",
        }}
      >
        <span className={`fas fa-${icon}`}>&nbsp;</span>
      </span>
    );
  } else {
    return (
      <span
        style={{
          border: "1px solid grey",
          borderRadius: "2px",
          padding: "3px",
          width: "24px",
          display: "inline-block",
          lineHeight: 1,
          marginRight: ".6em",
          backgroundColor:
            {
              boolean: "#fffbc6",
              integer: "#e8ffdd",
              real: "#e7f9cf",
              text: "#fff2f2",
              date: "#e8f2ff",
            }[column.type] || "transparent",
        }}
        data-toggle="tooltip"
        title={`${column.type}`}
      >
        {column.type[0].toUpperCase()}
      </span>
    );
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
              {potentiallyRenderColumnIcon(item)}
              <Higlighted table={table} column={item} field="name" />{" "}
              {item.key_type && (
                <>
                  {" ("}
                  <Higlighted table={table} column={item} field="key_type" />
                  {")"}
                </>
              )}
            </button>
            {item.analysis && <SparklineOverview item={item} />}
          </li>
        );
      })}
    </ul>
  );
};
/*
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
              
              <td>
                <Higlighted table={table} column={item} field="type" />
                {item.key_type && (
                  <>
                    {" ("}
                    <Higlighted table={table} column={item} field="key_type" />
                    {")"}
                  </>
                )}
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );
};*/
