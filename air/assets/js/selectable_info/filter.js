// @flow

import type { Column } from "./columns";

export class Filter {
  constructor(regex: RegExp) {
    this.regex = regex;

    this.anyColumnMatches = this.anyColumnMatches.bind(this);
    this.filterColumns = this.filterColumns.bind(this);
    this.filterFun = this.filterFun.bind(this);
  }

  regex: RegExp;

  filterFun = (column: Column): boolean =>
    this.regex.test(column.name) || this.regex.test(column.type);

  anyColumnMatches = (columns: Column[]): boolean =>
    columns.find(this.filterFun) !== undefined;

  filterColumns = (columns: Column[]): Column[] =>
    columns.filter(this.filterFun);
}

export const EmptyFilter = () => new Filter(new RegExp(""));
