// @flow

import React from "react";
import ReactDOM from "react-dom";

import {SelectableView} from "./selectable";
import {FilterView, Filter, EmptyFilter} from "./filter";
import type {Selectable} from "./selectable";

type Props = {readOnly: boolean, selectables: Selectable[]};

class SelectableInfo extends React.Component {
  props: Props;
  state: {expanded: Set<string>, filter: Filter, filterVisible: boolean};
  toggleExpand: (t: Selectable) => (() => void);
  onFilterChange: (filter: Filter) => void;
  toggleFilterVisibility: () => void;

  constructor(props) {
    super(props);

    this.state = {
      expanded: new Set(),
      filter: new EmptyFilter(),
      filterVisible: false,
    };

    this.toggleExpand = this.toggleExpand.bind(this);
    this.onFilterChange = this.onFilterChange.bind(this);
    this.toggleFilterVisibility = this.toggleFilterVisibility.bind(this);
  }

  onFilterChange(filter: Filter) {
    this.setState({filter});
  }

  toggleExpand(selectable) {
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

  expanded(selectable) {
    return this.state.expanded.has(selectable.id);
  }

  selectables() {
    return this.props.selectables;
  }

  conditionallyRenderFilter() {
    if (this.state.filterVisible) {
      return <FilterView onFilterChange={this.onFilterChange} handleClose={this.toggleFilterVisibility} />;
    } else {
      return null;
    }
  }

  conditionallyRenderFilterToggle() {
    if (! this.state.filterVisible) {
      return (<span
        onClick={this.toggleFilterVisibility}
        className="pull-right glyphicon glyphicon-search"
        aria-hidden="true"
      />);
    } else {
      return null;
    }
  }

  toggleFilterVisibility() {
    if (this.state.filterVisible) {
      this.setState({filter: new EmptyFilter(), filterVisible: false});
    } else {
      this.setState({filterVisible: true});
    }
  }

  render() {
    return (
      <div>
        {this.conditionallyRenderFilter()}

        <div className="panel panel-default selectable-info">
          <div className="panel-heading">
            <strong>Tables and views</strong>
            {this.conditionallyRenderFilterToggle()}
          </div>

          <div className="selectable-info-content">
            {this.selectables().map((selectable, i) =>
              <
                SelectableView
                key={i}
                readOnly={this.props.readOnly}
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

export default function renderSelectableInfo(data: Props, elem: Node) {
  ReactDOM.render(<SelectableInfo {...data} />, elem);
}
