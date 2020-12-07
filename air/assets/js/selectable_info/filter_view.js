// @flow

import type { Element } from "React";
import React from "react";
import { isEmptyFilter, emptyFilter } from "./filter";
import type { Filter } from "./filter";
import { Popover, OverlayTrigger } from "react-bootstrap";

type Props = {
  onFilterChange: (Filter) => void,
  filter: Filter,
};

const regularTypes = ["text", "date", "datetime", "real", "integer", "boolean"];

const specialTypes = { user_id: "user", foreign: "link" };

const FilterDetails = React.forwardRef(
  ({ filter, onFilterChange, style = {}, ...props }, ref) => {
    const typeCheckbox = (type, icon) => (
      <span className="type-filter" key={type}>
        <input
          type="checkbox"
          checked={filter.types[type]}
          value={type}
          id={`type-${type}`}
          onChange={(e) =>
            onFilterChange({
              ...filter,
              types: {
                ...filter.types,
                [(type: string)]: e.target.checked,
              },
            })
          }
        />
        <label htmlFor={`type-${type}`}>
          <span className={`type-icon native-type`}>{icon}</span>
          {type}
        </label>
      </span>
    );
    return (
      <Popover
        {...props}
        ref={ref}
        style={{
          ...style,
          minWidth: "290px",
        }}
      >
        <Popover.Content>
          <h4 className="h5">Search Options</h4>
          <div>
            <label>Types</label>
          </div>
          <div className="mb-2 type-filters">
            {regularTypes.map((type) =>
              typeCheckbox(type, type[0].toUpperCase())
            )}
            {Object.keys(specialTypes).map((type) =>
              typeCheckbox(
                type,
                <i className={`fas fa-${specialTypes[type]}`} />
              )
            )}
          </div>

          <div className="custom-control custom-switch">
            <input
              value="fuzzy"
              checked={filter.fuzzy}
              onChange={(e) =>
                onFilterChange({ ...filter, fuzzy: e.target.checked })
              }
              type="checkbox"
              className="custom-control-input"
              id="fuzzy"
            />
            <label className="custom-control-label" htmlFor="fuzzy">
              Use fuzzy search
            </label>
          </div>
        </Popover.Content>
      </Popover>
    );
  }
);

const FilterView = ({ filter, onFilterChange }: Props): Element<"div"> => (
  <div className="column-filter my-3 flex-shrink-0">
    <div className="input-group input-group-sm">
      <div className="input-group-prepend">
        <OverlayTrigger
          trigger="click"
          placement="bottom"
          rootClose={true}
          overlay={
            <FilterDetails filter={filter} onFilterChange={onFilterChange} />
          }
        >
          <button
            className={`btn ${
              Object.values(filter.types).every((a) => a)
                ? "btn-secondary"
                : "btn-primary"
            }`}
          >
            <i className="fas fa-search"></i>
          </button>
        </OverlayTrigger>
      </div>
      <input
        onChange={(event) =>
          onFilterChange({ ...filter, query: event.target.value })
        }
        type="text"
        className="form-control"
        placeholder="Filter"
        value={filter.query}
      />
      {!isEmptyFilter(filter) && (
        <div className="input-group-append">
          <button
            className="btn btn-outline-secondary"
            onClick={() => onFilterChange(emptyFilter())}
          >
            <span className="fas fa-times" aria-hidden="true" />
          </button>
        </div>
      )}
    </div>
  </div>
);

export default FilterView;
