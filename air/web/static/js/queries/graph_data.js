// @flow

import _ from "lodash";
import type {Row, Column} from "./result";

type ValueFormatter = (value: any) => any;

type Series = {label: Column, data: number[]};

export type GraphDataT = {
  ready: () => boolean,
  x: () => Column[],
  series: () => Series[],
};

export type GraphInfoT = {
  xColumns: () => Column[],
  usableAsY: (col: Column) => boolean,
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

  const usableAsY = (column) => {
    const index = columns.findIndex((x) => x === column);
    return isNumeric(rows[0].row[index]);
  };

  const chartable = () =>
    columns.length >= 2 &&
    rows.length > 1 &&
    rows.length <= 1000 &&
    _.some(columns, usableAsY);

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

  _xColumns: Set<Column>;
  _yColumns: Set<Column>;


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  xColumns() { return [...this._xColumns]; }

  yColumns() { return [...this._yColumns]; }

  addX(col: Column) { this._yColumns.delete(col); this._xColumns.add(col); return this; }

  addY(col: Column) { this._xColumns.delete(col); this._yColumns.add(col); return this; }

  remove(col: Column) { this._xColumns.delete(col); this._yColumns.delete(col); return this; }
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

  const indices = {};
  columns.forEach((column, index) => { indices[column] = index; });

  const valueFormatter = formatter || _.identity;


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  const ready = () => graphConfig.xColumns().length > 0 && graphConfig.yColumns().length > 0;

  const x = () => rows.map(({row}) =>
    graphConfig.xColumns().map((column) =>
      row[indices[column]]).
        map(valueFormatter).
        join(", ")
    );

  const series = () => graphConfig.yColumns().map((column) => ({
    label: column,
    data: rows.map(({row}) => row[indices[column]]),
  }));

  return {ready, x, series};
};
