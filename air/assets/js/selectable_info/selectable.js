// @flow

import React from "react";
import Channel from "phoenix";

import { ColumnsView } from "./columns";
import { Filter } from "./filter";
import type { Column } from "./columns";
import activateTooltips from "../tooltips";

export type Selectable = {
  id: string,
  internal_id: number,
  kind: string,
  columns: Column[],
  delete_html: string,
  broken: boolean,
  creation_status: string
};

type Props = {
  selectable: Selectable,
  selectablesEditUrl: string,
  onClick: () => void,
  expanded: boolean,
  filter: Filter,
  channel: Channel
};

const ERROR_REASON_MESSAGE =
  "This might be caused by a change in the underlying data source, " +
  "a dependent analyst table, or a view.";

const VIEW_INVALID_MESSAGE = `This view is no longer valid. ${ERROR_REASON_MESSAGE}`;

const TABLE_INVALID_MESSAGE =
  `This table creation failed or the table is no longer valid. ${ERROR_REASON_MESSAGE}. ` +
  "More information may be available in Insights Cloak logs - contact your administrator for access.";

export class SelectableView extends React.Component<Props> {
  constructor(props: Props) {
    super(props);

    this.handleToggleClick = this.handleToggleClick.bind(this);
    this.isAnalystCreatedSelectable = this.isAnalystCreatedSelectable.bind(
      this
    );
    this.hasRenderableContent = this.hasRenderableContent.bind(this);
    this.renderSelectableActionMenu = this.renderSelectableActionMenu.bind(
      this
    );
    this.renderSelectableView = this.renderSelectableView.bind(this);
    this.triggerDelete = this.triggerDelete.bind(this);
    this.brokenErrorMessage = this.brokenErrorMessage.bind(this);
  }

  handleToggleClick = (event: {
    target: Element,
    preventDefault: () => void
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

  isAnalystCreatedSelectable = () => {
    const { selectable } = this.props;
    return selectable.kind === "view" || selectable.kind === "analyst_table";
  };

  hasRenderableContent = () => {
    const { filter, selectable } = this.props;
    return filter.anyColumnMatches(selectable.columns);
  };

  editLinkUrl = () => {
    const { selectable, selectablesEditUrl } = this.props;
    return `${selectablesEditUrl}?kind=${selectable.kind}&id=${selectable.internal_id}`;
  };

  triggerDelete = (event: { preventDefault: () => void }) => {
    const { selectable, channel } = this.props;
    if (window.confirm(`Do you want to permanently delete ${selectable.id}?`)) {
      // eslint-disable-line no-alert
      channel.push("delete_selectable", {
        internal_id: selectable.internal_id,
        kind: selectable.kind
      });
    }
    event.preventDefault();
  };

  renderSelectableActionMenu = () => {
    if (this.pending()) {
      return null;
    } else {
      return (
        <span className="pull-right">
          &nbsp;
          <button
            type="button"
            className="btn btn-xs btn-default"
            href={this.editLinkUrl()}
          >
            Edit
          </button>
          &nbsp;
          <button
            type="button"
            className="btn btn-xs btn-danger"
            onClick={this.triggerDelete}
          >
            Delete
          </button>
        </span>
      );
    }
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
        className: "list-group-item-heading alert-danger"
      };
    } else {
      return {
        title: null,
        dataToggle: null,
        dataContainer: null,
        className: "list-group-item-heading"
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
          src="/images/loader.gif"
          alt="indicated analyst table is being created"
          height="12"
          width="12"
        />
      );
    } else {
      const glyphType = expanded
        ? "glyphicon glyphicon-minus"
        : "glyphicon glyphicon-plus";
      return <span className={glyphType} />;
    }
  };

  renderSelectableView = () => {
    const { selectable, expanded, filter } = this.props;
    const {
      title,
      dataToggle,
      dataContainer,
      className
    } = this.brokenMetaData();
    return (
      <div className="list-group-item">
        <div
          role="button"
          tabIndex={0}
          onKeyDown={this.handleToggleClick}
          onClick={this.handleToggleClick}
          title={title}
          data-container={dataContainer}
          data-toggle={dataToggle}
          className={className}
        >
          {this.renderIcon()}
          &nbsp;
          {selectable.id}
          {this.isAnalystCreatedSelectable()
            ? this.renderSelectableActionMenu()
            : null}
        </div>

        {(() => {
          if (expanded) {
            return <ColumnsView columns={selectable.columns} filter={filter} />;
          } else {
            return null;
          }
        })()}
      </div>
    );
  };

  render = () => {
    if (this.hasRenderableContent()) {
      activateTooltips();
      return this.renderSelectableView();
    } else {
      return null;
    }
  };
}
