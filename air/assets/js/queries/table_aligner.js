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

  const firstResultCell = (columnIndex) => {
    const firstValue = _.find(rows, (rowData) => ! _.includes(ignorableValues, rowData.row[columnIndex]));
    if (firstValue === undefined) {
      return undefined;
    } else {
      return firstValue.row[columnIndex];
    }
  };


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
