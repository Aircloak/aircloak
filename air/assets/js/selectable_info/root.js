// @flow

import React from "react";

import {SelectableView} from "./selectable";
import {FilterView, Filter, EmptyFilter} from "./filter";
import type {Selectable} from "./selectable";

type Props = {
  selectables: Selectable[],
  newViewURL: string,
};

export default class SelectableInfo extends React.Component {
  props: Props;
  state: {expanded: Set<string>, filter: Filter};
  toggleExpand: (t: Selectable) => (() => void);
  onFilterChange: (filter: Filter) => void;

  constructor(props: Props) {
    super(props);

    this.state = {
      expanded: new Set(),
      filter: new EmptyFilter(),
    };

    this.toggleExpand = this.toggleExpand.bind(this);
    this.onFilterChange = this.onFilterChange.bind(this);
  }

  onFilterChange(filter: Filter) {
    this.setState({filter});
  }

  toggleExpand(selectable: Selectable) {
    return () => {
      const expanded = this.state.expanded;
      if (this.expanded(selectable)) {
        expanded.delete(selectable.id);
      } else {
        expanded.add(selectable.id);
      }
      this.setState({expanded});
    };
  }

  expanded(selectable: Selectable) {
    return this.state.expanded.has(selectable.id);
  }

  selectables() {
    return this.props.selectables;
  }

  render() {
    return (
      <div>
        <div className="panel panel-default selectable-info">
          <div className="panel-heading selectable-heading">
            <a href={this.props.newViewURL} className="btn btn-default btn-xs pull-right">
              New view
            </a>
            <strong>Tables and views</strong>
          </div>

          <FilterView onFilterChange={this.onFilterChange} />

          <div className="selectable-info-content">
            {this.selectables().map((selectable, i) =>
              <
                SelectableView
                key={i}
                filter={this.state.filter}
                selectable={selectable}
                expanded={this.expanded(selectable)}
                onClick={this.toggleExpand(selectable)}
              />
            )}
          </div>
        </div>
      </div>
    );
  }
}
