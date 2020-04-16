// @flow

import React from "react";

type Props = {
  onFilterChange: (string) => void,
  filter: string,
};

const FilterView = ({ filter, onFilterChange }: Props) => (
  <div className="column-filter">
    <div className="input-group">
      <input
        onChange={(event) => onFilterChange(event.target.value)}
        type="text"
        className="form-control"
        placeholder="Filter columns"
        value={filter}
      />
      <div className="input-group-append">
        <button
          className="btn btn-outline-secondary"
          onClick={() => onFilterChange("")}
        >
          <span className="fas fa-times" aria-hidden="true" />
        </button>
      </div>
    </div>
  </div>
);

export default FilterView;
