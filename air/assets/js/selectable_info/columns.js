// @flow

import React from "react";

import { Higlighted, needsHighlighting } from "./filter";
import ColumnInfo from "./column-info.js";
import type { NumberFormat } from "../number_format";
import ColumnIcon from "./column-icon";

export type Column = {
  name: string,
  type: string,
  key_type: string,
  analysis?: any,
  comment: string | null,
  access: "visible" | "unselectable",
  _indexEntry?: any,
  _matchIndexes?: {
    type: number[],
    name: number[],
    table: number[],
  },
};

export const ColumnsView = ({
  columns,
  table,
  numberFormat,
}: {
  table: string,
  columns: Column[],
  numberFormat: NumberFormat,
}) => {
  return (
    <ul className="list-group list-group-flush">
      {columns.map((item) => {
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
              <ColumnIcon column={item} highlight={needsHighlighting(item)} />
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
