// @flow

import _ from "lodash";

import type {Column} from "./columns";

export class Filter {
  constructor(regex: RegExp) {
    this.regex = regex;

    this.anyColumnMatches = this.anyColumnMatches.bind(this);
    this.filterColumns = this.filterColumns.bind(this);
    this.filterFun = this.filterFun.bind(this);
  }

  regex: RegExp;

  filterFun = (column: Column): boolean => this.regex.test(column.name) || this.regex.test(column.type)

  anyColumnMatches = (columns: Column[]): boolean => _.find(columns, this.filterFun) !== undefined

  filterColumns = (columns: Column[]): Column[] => _.filter(columns, this.filterFun)
}

export const EmptyFilter = () => new Filter(new RegExp(""));
