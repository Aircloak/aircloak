// @flow

import React from "react";

import { filterColumns, Higlighted } from "./filter";
import ColumnInfo from "./column-info.js";
import type { NumberFormat } from "../number_format";
import ColumnIcon from "./column-icon";

export type Column = {
  name: string,
  type: string,
  key_type: string,
  analysis?: any,
  comment: string | null,
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
            className={`list-group-item p-1 bg-transparent pr-3 d-flex justify-content-between align-items-center`}
            key={item.name}
          >
            <button
              className="btn text-truncate"
              style={{ textAlign: "left" }}
              onClick={(event) => {
                event.preventDefault();
                window.insertWordInEditor(`"${item.name}"`);
              }}
            >
              <ColumnIcon column={item} />
              <Higlighted table={table} column={item} field="name" />{" "}
              {item.key_type && (
                <>
                  {" ("}
                  <Higlighted table={table} column={item} field="key_type" />
                  {")"}
                </>
              )}
            </button>
            <span className="align-items-baseline d-flex">
              <ColumnInfo item={item} numberFormat={numberFormat} />
            </span>
          </li>
        );
      })}
    </ul>
  );
};
