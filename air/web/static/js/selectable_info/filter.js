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
  filterFun: () => RegExp;

  constructor(filterText: string) {
    this.compileRegex(filterText);

    this.anyColumnMatches = this.anyColumnMatches.bind(this);
    this.filterColumns = this.filterColumns.bind(this);
    this.filterFun = this.filterFun.bind(this);
  }

  compileRegex(filterText: string) {
    if (filterText !== "") {
      try {
        this.regex = new RegExp(filterText);
      } catch (SyntaxError) {
        // If the regular expression doesn't compile, it's
        // probably due to it not being finished yet, hence we
        // ignore the error, and don't compile the regex.
      }
    }
  }

  filterFun(column: Column) {
    return this.regex === undefined || this.regex.test(column.name) || this.regex.test(column.type);
  }

  anyColumnMatches(columns: Column[]): boolean {
    return _.find(columns, this.filterFun) !== undefined;
  }

  filterColumns(columns: Column[]): Column[] {
    return _.filter(columns, this.filterFun);
  }
}

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
    this.props.onFilterChange(new Filter(filterText));
  }

  render() {
    return (
      <div className="list-group-item">
        <form className="form-inline">
          <div className="form-group">
            <div className="input-group">
              <div className="input-group-addon">
                <span
                  className="glyphicon glyphicon-search"
                  aria-hidden="true"
                  aria-label="Filter columns"
                />
              </div>
              <input
                onChange={(event) => this.filterTextChange(event.target.value)}
                type="text"
                className="form-control"
                placeholder="Filter columns"
                value={this.state.filterText}
              />
            </div>
          </div>
        </form>
      </div>
    );
  }
}
