// @flow

import React from "react";
import _ from "lodash";
import Channel from "phoenix";

import {SelectableView} from "./selectable";
import {NewSelectableToolbarView} from "./new_selectable_toolbar";
import {FilterView, Filter, EmptyFilter} from "./filter";
import {FrontendSocket} from "../frontend_socket";
import type {Selectable} from "./selectable";

type Props = {
  selectables: Selectable[],
  selectablesEditUrl: string,
  newTableURL: string,
  newViewURL: string,
  userId: number,
  dataSourceName: string,
  dataSourceDescription: ?string,
  dataSourceStatus: string,
  frontendSocket: FrontendSocket,
  supportsCreateTable: boolean,
  selectableToExclude: number,
};

type State = {
  expanded: Set<string>,
  filter: Filter,
  selectables: Selectable[],
  dataSourceStatus: string,
}

export default class SelectableInfo extends React.Component<Props, State> {
  toggleExpand: (t: Selectable) => (() => void);
  onFilterChange: (filter: Filter) => void;
  updateSelectables: (event: {selectables: Selectable[]}) => void;
  channel: Channel;

  constructor(props: Props) {
    super(props);

    this.state = {
      expanded: new Set(),
      filter: new EmptyFilter(),
      selectables: props.selectables,
      dataSourceStatus: this.props.dataSourceStatus,
    };

    this.toggleExpand = this.toggleExpand.bind(this);
    this.onFilterChange = this.onFilterChange.bind(this);
    this.updateSelectables = this.updateSelectables.bind(this);

    this.channel = this.props.frontendSocket.joinSelectablesChannel(
      this.props.dataSourceName, this.props.userId, {
        handleEvent: (event) => this.updateSelectables(event),
        joined: (event) => this.updateSelectables(event),
      }
    );

    this.props.frontendSocket.joinDataSourceChannel(this.props.dataSourceName, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
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

  updateSelectables(event: {selectables: Selectable[]}) {
    this.setState({selectables: event.selectables});
  }

  expanded(selectable: Selectable) {
    return this.state.expanded.has(selectable.id);
  }

  selectables() {
    return _.reject(this.state.selectables, (selectable) =>
      selectable.internal_id === (this.props.selectableToExclude || "don't exclude any"));
  }

  dataSourceStatusReceived(event: { status: string }) {
    this.setState({dataSourceStatus: event.status});
  }


  renderAvailabilityLabel() {
    switch (this.state.dataSourceStatus) {
      case "online": return <span className="label label-success pull-right">Online</span>;
      case "offline": return <span className="label label-danger pull-right">Offline</span>;
      case "analyzing": return this.analyzing();
      default: return <span className="label label-warning pull-right">Broken</span>;
    }
  }

  analyzing() {
    return (<span className="label label-success pull-right">
      Online
      <sup>
        <a
          href="/docs/sql/restrictions.html#column-analysis"
          target="blank"
          data-toggle="tooltip"
          data-placement="right"
          title="Some features unavailable pending analysis"
        >
          *
        </a>
      </sup>
    </span>);
  }

  renderDataSourceDescription() {
    if (this.props.dataSourceDescription) {
      return <p>{this.props.dataSourceDescription}</p>;
    } else {
      return null;
    }
  }

  render() {
    return (
      <div className="panel panel-default selectable-info">

        <div className="panel-heading selectable-heading">
          <strong>{this.props.dataSourceName}</strong>
          {this.renderAvailabilityLabel()}
          {this.renderDataSourceDescription()}
        </div>

        <FilterView onFilterChange={this.onFilterChange} />

        <div className="selectable-info-content">
          {this.selectables().map((selectable, i) =>
            <
              SelectableView
              key={i}
              filter={this.state.filter}
              selectable={selectable}
              selectablesEditUrl={this.props.selectablesEditUrl}
              channel={this.channel}
              expanded={this.expanded(selectable)}
              onClick={this.toggleExpand(selectable)}
            />
          )}
        </div>

        <div className="panel-footer">
          <NewSelectableToolbarView
            newTableURL={this.props.newTableURL}
            newViewURL={this.props.newViewURL}
            supportsCreateTable={this.props.supportsCreateTable}
          />
        </div>
      </div>
    );
  }
}
