// @flow

import type { Element } from "React";
import React from "react";
import type { Channel } from "phoenix";

import { SelectableView } from "./selectable";
import FilterView from "./filter_view";
import FrontendSocket from "../frontend_socket";
import type { Selectable } from "./selectable";
import type { NumberFormat } from "../number_format";
import { selectableType } from "./selectable-type";
import { emptyFilter } from "./filter";
import type { Filter } from "./filter";

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
  numberFormat: NumberFormat,
};

type State = {
  expanded: Set<string>,
  filter: Filter,
  selectables: Selectable[],
  dataSourceStatus: string,
};

export default class SelectableInfo extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    const {
      dataSourceStatus,
      frontendSocket,
      dataSourceName,
      userId,
    } = this.props;

    this.state = {
      expanded: new Set(),
      filter: emptyFilter(),
      selectables: props.selectables,
      dataSourceStatus,
    };

    this.toggleExpand = this.toggleExpand.bind(this);
    this.onFilterChange = this.onFilterChange.bind(this);
    this.updateSelectables = this.updateSelectables.bind(this);

    this.channel = frontendSocket.joinSelectablesChannel(
      dataSourceName,
      userId,
      {
        handleEvent: (event) => this.updateSelectables(event),
        joined: (event) => this.updateSelectables(event),
      }
    );

    frontendSocket.joinDataSourceChannel(dataSourceName, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
  }

  channel: Channel;

  onFilterChange: any | ((filter: Filter) => void) = (filter: Filter) => {
    this.setState({ filter });
  };

  toggleExpand: any | ((selectable: Selectable) => () => void) = (
    selectable: Selectable
  ) => () => {
    this.setState((state) => {
      const expanded = state.expanded;
      if (this.expanded(selectable)) {
        expanded.delete(selectable.id);
      } else {
        expanded.add(selectable.id);
      }
      return { expanded };
    });
  };

  updateSelectables:
    | any
    | ((event: { selectables: Array<Selectable>, ... }) => void) = (event: {
    selectables: Selectable[],
  }) => {
    this.setState({ selectables: event.selectables });
  };

  expanded: (selectable: Selectable) => boolean = (selectable: Selectable) =>
    this.state.expanded.has(selectable.id);

  selectables: () => Array<[string, any]> = (): Array<[string, any]> =>
    Object.entries(
      this.state.selectables
        .filter(
          (selectable) =>
            selectable.internal_id !==
            (this.props.selectableToExclude || "don't exclude any")
        )
        .reduce(
          (groups, selectable) => {
            const type = selectableType(selectable);

            return { ...groups, [type]: [...(groups[type] || []), selectable] };
          },
          this.props.supportsCreateTable
            ? { View: [], "Analyst Table": [] }
            : { View: [] }
        )
    );

  dataSourceStatusReceived: (event: { status: string, ... }) => void = (event: {
    status: string,
  }) => this.setState({ dataSourceStatus: event.status });

  renderAvailabilityLabel: () => Element<"span"> = () => {
    switch (this.state.dataSourceStatus) {
      case "online":
        return <span className="badge badge-success ml-2">Online</span>;
      case "offline":
        return <span className="badge badge-danger ml-2">Offline</span>;
      case "analyzing":
        return this.analyzing();
      default:
        return <span className="badge badge-warning ml-2">Broken</span>;
    }
  };

  analyzing: () => Element<"span"> = () => (
    <span className="badge badge-success ml-2">
      Online
      <a
        href="/docs/#/sql/restrictions?id=column-analysis"
        target="blank"
        data-toggle="tooltip"
        data-placement="bottom"
        className="text-dark"
        title="Some features unavailable pending analysis"
      >
        *
      </a>
    </span>
  );

  renderDataSourceDescription: () => null | Element<"p"> = () => {
    const { dataSourceDescription } = this.props;
    if (dataSourceDescription) {
      return <p>{dataSourceDescription}</p>;
    } else {
      return null;
    }
  };

  render: () => Element<"div"> = () => {
    const {
      dataSourceName,
      selectablesEditUrl,
      newTableURL,
      newViewURL,
      supportsCreateTable,
      numberFormat,
    } = this.props;
    const { filter } = this.state;
    return (
      <div
        id="sidebar"
        className="sidebar fixed-right border-left navbar-expand-md collapse"
      >
        <div className="d-flex flex-column h-100">
          <div className="sidebar-header d-flex justify-content-between align-items-center flex-shrink-0">
            <h1 className="h3 m-0 text-truncate">{dataSourceName}</h1>
            {this.renderAvailabilityLabel()}

            <button
              type="button"
              className="btn d-block d-md-none"
              data-toggle="collapse"
              data-target="#sidebar"
              aria-expanded="false"
            >
              <i className="fas fa-times" aria-label="Hide sidebar"></i>
            </button>
          </div>
          {this.renderDataSourceDescription()}

          <FilterView filter={filter} onFilterChange={this.onFilterChange} />

          <div className="selectable-info-content flex-grow-1 overflow-auto list-group list-group-flush mx-n4">
            {this.selectables().map(([category, selectables]) => (
              <div key={category}>
                <div className="d-flex justify-content-between px-4 py-2 bg-white sticky-top align-items-baseline border-bottom">
                  <h4 className="h6 text-uppercase small font-weight-bold text-muted m-1">
                    {category}
                  </h4>
                  {category === "View" && (
                    <a
                      href={newViewURL}
                      className="btn btn-link btn-sm p-0 m-0"
                    >
                      <i className="fas fa-plus"></i> New
                    </a>
                  )}
                  {category === "Analyst Table" && (
                    <a
                      href={newTableURL}
                      className="btn btn-link btn-sm p-0 m-0"
                    >
                      <i className="fas fa-plus"></i> New
                    </a>
                  )}
                </div>
                {Array.from(selectables).map((selectable) => (
                  <SelectableView
                    key={selectable.id}
                    filter={filter}
                    selectable={selectable}
                    selectablesEditUrl={selectablesEditUrl}
                    channel={this.channel}
                    expanded={this.expanded(selectable)}
                    onClick={this.toggleExpand(selectable)}
                    numberFormat={numberFormat}
                    newTableURL={newTableURL}
                    newViewURL={newViewURL}
                    supportsCreateTable={supportsCreateTable}
                  />
                ))}
              </div>
            ))}
          </div>
        </div>
      </div>
    );
  };
}
