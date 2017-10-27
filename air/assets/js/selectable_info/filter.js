// @flow

import React from "react";
import _ from "lodash";

import type {Column} from "./columns";

type Props = {
  value: string,
  onFilterChange: () => void,
};

export class Filter {
  regex: RegExp;
  anyColumnMatches: (columns: Column[]) => boolean;
  filterColumns: (columns: Column[]) => Column[];
  filterFun: (column: Column) => boolean;

  constructor(regex: RegExp) {
    this.regex = regex;

    this.anyColumnMatches = this.anyColumnMatches.bind(this);
    this.filterColumns = this.filterColumns.bind(this);
    this.filterFun = this.filterFun.bind(this);
  }

  filterFun(column: Column): boolean {
    return this.regex.test(column.name) || this.regex.test(column.type);
  }

  anyColumnMatches(columns: Column[]): boolean {
    return _.find(columns, this.filterFun) !== undefined;
  }

  filterColumns(columns: Column[]): Column[] {
    return _.filter(columns, this.filterFun);
  }
}

export const EmptyFilter = () =>
  new Filter(new RegExp(""));

export class FilterView extends React.Component {
  state: {filterText: string};
  filterTextChange: () => void;

  constructor(props: Props) {
    super(props);

    this.state = {
      filterText: "",
    };

    this.filterTextChange = this.filterTextChange.bind(this);
  }

  filterTextChange(filterText: string) {
    this.setState({filterText});

    try {
      const regex = new RegExp(filterText);
      this.props.onFilterChange(new Filter(regex));
    } catch (SyntaxError) {
      // If the regular expression doesn't compile, it's
      // probably due to it not being finished yet, hence we
      // ignore the error, and leave the old filter in place.
    }
  }

  render() {
    return (
      <div className="column-filter">
        <div className="input-group">
          <input
            onChange={(event) => this.filterTextChange(event.target.value)}
            type="text"
            className="form-control"
            placeholder="Filter columns"
            value={this.state.filterText}
          />
          <div
            className="input-group-addon"
            onClick={() => this.filterTextChange("")}
          >
            <span className="glyphicon glyphicon-remove" aria-hidden="true" />
          </div>
        </div>
      </div>
    );
  }
}
