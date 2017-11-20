// @flow

import _ from "lodash";
import type {Row, Column} from "./result";

type ValueFormatter = (value: any, columnIndex: number) => any;

type Series = {label: Column, data: number[], indexInResult: number};

export type GraphDataT = {
  ready: () => boolean,
  x: () => Column[],
  xLabel: () => string,
  series: () => Series[],
};

export type GraphInfoT = {
  xColumns: () => Column[],
  usableAsY: (index: number) => boolean,
  chartable: () => boolean,
};

export const GraphInfo = (columns: Column[], rows: Row[]): GraphInfoT => {
  // ----------------------------------------------------------------
  // Internal functions
  // ----------------------------------------------------------------

  const isNumeric = (n) => typeof(n) === "number" && isFinite(n);


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  const xColumns = () => columns;

  const usableAsY = (index) => isNumeric(rows[0].row[index]);

  const chartable = () =>
    columns.length >= 2 &&
    rows.length > 1 &&
    _.some(columns, (_column, index) => usableAsY(index));

  return {xColumns, usableAsY, chartable};
};

export class GraphConfig {
  // ----------------------------------------------------------------
  // State
  // ----------------------------------------------------------------

  constructor() {
    this._xColumns = new Set();
    this._yColumns = new Set();
  }

  _xColumns: Set<number>;
  _yColumns: Set<number>;


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  xColumns() { return [...this._xColumns].sort(); }

  yColumns() { return [...this._yColumns].sort(); }

  addX(col: number) { this._yColumns.delete(col); this._xColumns.add(col); return this; }

  addY(col: number) { this._xColumns.delete(col); this._yColumns.add(col); return this; }

  remove(col: number) { this._xColumns.delete(col); this._yColumns.delete(col); return this; }
}

export const GraphData = (
  columns: Column[],
  rows: Row[],
  graphConfig: GraphConfig,
  formatter: ValueFormatter
): GraphDataT => {
  // ----------------------------------------------------------------
  // Internal functions
  // ----------------------------------------------------------------

  const valueFormatter = formatter || _.identity;


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  const ready = () => graphConfig.xColumns().length > 0 && graphConfig.yColumns().length > 0;

  const x = () => rows.map(({row}) =>
    graphConfig.xColumns().map((columnIndex) =>
        valueFormatter(row[columnIndex], columnIndex)
      ).join(", ")
    );

  const xLabel = () => graphConfig.xColumns().map((columnIndex) =>
    columns[columnIndex]
  ).join(", ");

  const series = () => graphConfig.yColumns().map((columnIndex) => ({
    label: columns[columnIndex],
    data: rows.map(({row}) => row[columnIndex]),
    indexInResult: columnIndex,
  }));

  return {ready, x, xLabel, series};
};
