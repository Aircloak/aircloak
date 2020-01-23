// @flow

import React from "react";

import { Filter } from "./filter";

type Props = {
  onFilterChange: Filter => void
};

type State = {
  filterText: string
};

export default class FilterView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {
      filterText: ""
    };

    this.filterTextChange = this.filterTextChange.bind(this);
  }

  filterTextChange = (filterText: string) => {
    this.setState({ filterText });

    try {
      const caseInsensitive = "i";
      const regex = new RegExp(filterText, caseInsensitive);
      const { onFilterChange } = this.props;
      onFilterChange(new Filter(regex));
    } catch (SyntaxError) {
      // If the regular expression doesn't compile, it's
      // probably due to it not being finished yet, hence we
      // ignore the error, and leave the old filter in place.
    }
  };

  render = () => {
    const { filterText } = this.state;
    return (
      <div className="column-filter">
        <div className="input-group">
          <input
            onChange={event => this.filterTextChange(event.target.value)}
            type="text"
            className="form-control"
            placeholder="Filter columns"
            value={filterText}
          />
          <div
            role="button"
            tabIndex={0}
            className="input-group-addon"
            onKeyDown={() => this.filterTextChange("")}
            onClick={() => this.filterTextChange("")}
          >
            <span className="glyphicon glyphicon-remove" aria-hidden="true" />
          </div>
        </div>
      </div>
    );
  };
}
