// @flow

import React from "react";
import ReactDOM from "react-dom";

import {SelectableView} from "./selectable";
import type {Selectable} from "./selectable";

type Props = {readOnly: boolean, selectables: Selectable[]};

class SelectableInfo extends React.Component {
  props: Props;
  state: {expanded: Set<string>};
  toggleExpand: (t: Selectable) => (() => void);

  constructor(props) {
    super(props);

    this.state = {expanded: new Set()};

    this.toggleExpand = this.toggleExpand.bind(this);
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

  render() {
    return (
      <div className="panel panel-default selectable-info">
        <div className="panel-heading"><strong>Tables and views</strong></div>

        <div className="selectable-info-content">
          {this.selectables().map((selectable, i) =>
            <
              SelectableView
              key={i}
              readOnly={this.props.readOnly}
              selectable={selectable}
              expanded={this.expanded(selectable)}
              onClick={this.toggleExpand(selectable)}
            />
          )}
        </div>
      </div>
    );
  }
}

export default function renderSelectableInfo(data: Props, elem: Node) {
  ReactDOM.render(<SelectableInfo {...data} />, elem);
}
