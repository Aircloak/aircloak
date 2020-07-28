// @flow

import React from "react";
import Channel from "phoenix";

import { ColumnsView } from "./columns";
import { isEmptyFilter, filterColumns, Higlighted } from "./filter";
import type { Filter } from "./filter";
import type { Column } from "./columns";
import activateTooltips from "../tooltips";
import loader from "../../static/images/loader.gif";
import type { NumberFormat } from "../number_format";
import SelectableInfo from "./selectable-info";

export type Selectable = {
  id: string,
  internal_id: number,
  content_type: "private" | "public" | null,
  kind: "table" | "view" | "analyst_table",
  columns: Column[],
  delete_html: string,
  broken: boolean,
  creation_status: string,
  comment: string | null,
};

type Props = {
  selectable: Selectable,
  selectablesEditUrl: string,
  onClick: () => void,
  expanded: boolean,
  filter: Filter,
  channel: Channel,
  numberFormat: NumberFormat,
};

const ERROR_REASON_MESSAGE =
  "This might be caused by a change in the underlying data source, " +
  "a dependent analyst table, or a view.";

const VIEW_INVALID_MESSAGE = `This view is no longer valid. ${ERROR_REASON_MESSAGE}`;

const TABLE_INVALID_MESSAGE =
  `This table creation failed or the table is no longer valid. ${ERROR_REASON_MESSAGE}. ` +
  "More information may be available in Insights Cloak logs - contact your administrator for access.";

export class SelectableView extends React.Component<Props> {
  handleToggleClick = (event: {
    target: Element,
    preventDefault: () => void,
  }) => {
    // Hacky solution to prevent bubbling from `<a>` elements. Normally, we'd use stopPropagation.
    // However, the problem here is that we're injecting some html provided by the server, which
    // internally generates A elements. Therefore, we don't have such option, so we're doing it
    // here.
    if (event.target.tagName !== "A" && !this.pending()) {
      event.preventDefault();
      const { onClick } = this.props;
      onClick();
    }
  };

  searchResults = () => {
    const { filter, selectable } = this.props;

    return filterColumns(selectable.id, selectable.columns, filter);
  };

  triggerDelete = (event: { preventDefault: () => void }) => {
    const { selectable, channel } = this.props;
    if (window.confirm(`Do you want to permanently delete ${selectable.id}?`)) {
      // eslint-disable-line no-alert
      channel.push("delete_selectable", {
        internal_id: selectable.internal_id,
        kind: selectable.kind,
      });
    }
    event.preventDefault();
  };

  brokenErrorMessage = () => {
    const { selectable } = this.props;
    if (selectable.kind === "view") {
      return VIEW_INVALID_MESSAGE;
    } else {
      return TABLE_INVALID_MESSAGE;
    }
  };

  brokenMetaData = () => {
    const { selectable } = this.props;
    if (selectable.broken || selectable.creation_status === "failed") {
      return {
        title: this.brokenErrorMessage(),
        dataToggle: "tooltip",
        dataContainer: "body",
        className: "list-group-item-heading alert-danger",
      };
    } else {
      return {
        title: null,
        dataToggle: null,
        dataContainer: null,
        className: "list-group-item-heading",
      };
    }
  };

  pending = () => {
    const { selectable } = this.props;
    return selectable.creation_status === "pending";
  };

  renderIcon = () => {
    const { expanded } = this.props;
    if (this.pending()) {
      return (
        <img
          src={loader}
          alt="analyst table is being created"
          height="12"
          width="12"
        />
      );
    } else {
      const glyphType = expanded
        ? "fas fa-caret-right fa-rotate-90"
        : "fas fa-caret-right";
      return <span className={glyphType} />;
    }
  };

  renderSelectableView = (searchResults: any) => {
    const {
      selectable,
      expanded,
      filter,
      numberFormat,
      selectablesEditUrl,
    } = this.props;
    const {
      title,
      dataToggle,
      dataContainer,
      className,
    } = this.brokenMetaData();

    return (
      <div className="list-group-item px-4 py-1 bg-transparent border-left-0">
        <div className="d-flex justify-content-between align-items-baseline">
          <button
            onClick={this.handleToggleClick}
            title={title}
            data-container={dataContainer}
            data-toggle={dataToggle}
            className={`${className} btn ml-n2 text-truncate text-left`}
          >
            {this.renderIcon()}
            <span className="pl-2">
              <Higlighted
                table={selectable.id}
                column={searchResults[0]}
                field="table"
              />
              {!isEmptyFilter(filter) && (
                <span className="badge badge-warning badge-pill ml-1">
                  {searchResults.length}{" "}
                  {searchResults.length > 1 ? "results" : "result"}
                </span>
              )}
            </span>
          </button>
          <SelectableInfo
            selectable={selectable}
            selectablesEditUrl={selectablesEditUrl}
            triggerDelete={this.triggerDelete}
          />
        </div>

        {expanded && (
          <ColumnsView
            table={selectable.id}
            columns={searchResults}
            numberFormat={numberFormat}
          />
        )}
      </div>
    );
  };

  render = () => {
    const results = this.searchResults();
    if (results.length > 0) {
      activateTooltips();
      return this.renderSelectableView(results);
    } else {
      return null;
    }
  };
}
