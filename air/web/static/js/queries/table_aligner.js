// @flow

import _ from "lodash";
import type {Row} from "./result";

export type TableAlignerT = {
  alignmentClass: (columnIndex: number) => string,
};

export const TableAligner = (rows: Row[]): TableAlignerT => {
  // ----------------------------------------------------------------
  // Internal functions
  // ----------------------------------------------------------------

  const isNumeric = (n) => typeof(n) === "number" && isFinite(n);

  const ignorableValues = ["*", null, undefined];

  const firstResultCell = (columnIndex) =>
    _.chain(rows).
      dropWhile((rowData) => _.includes(ignorableValues, rowData.row[columnIndex])).
      head().
      value().
      row[columnIndex];


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  const alignmentClass = (columnIndex) => {
    if (isNumeric(firstResultCell(columnIndex))) {
      return "text-right";
    } else {
      return "text-left";
    }
  };

  return {alignmentClass};
};
