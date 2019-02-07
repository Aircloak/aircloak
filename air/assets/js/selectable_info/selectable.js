// @flow

import React from "react";
import Channel from "phoenix";

import {ColumnsView} from "./columns";
import {Filter} from "./filter";
import type {Column} from "./columns";
import {activateTooltips} from "../tooltips";

export type Selectable = {
  id: string,
  internal_id: number,
  kind: string,
  columns: Column[],
  delete_html: string,
  broken: boolean,
  creation_status: string,
};

type Props = {
  selectable: Selectable,
  selectablesEditUrl: string,
  onClick: () => void,
  expanded: boolean,
  filter: Filter,
  channel: Channel
};

const VIEW_INVALID_MESSAGE =
  "This view is no longer valid. The underlying data source or another view might have changed to cause this.";

export class SelectableView extends React.Component {
  handleToggleClick: () => void;
  isAnalystCreatedSelectable: () => boolean;
  renderSelectableActionMenu: () => void;
  renderSelectableView: () => void;
  hasRenderableContent: () => boolean;
  triggerDelete: (event: {preventDefault: () => void}) => void;

  constructor(props: Props) {
    super(props);

    this.handleToggleClick = this.handleToggleClick.bind(this);
    this.isAnalystCreatedSelectable = this.isAnalystCreatedSelectable.bind(this);
    this.hasRenderableContent = this.hasRenderableContent.bind(this);
    this.renderSelectableActionMenu = this.renderSelectableActionMenu.bind(this);
    this.renderSelectableView = this.renderSelectableView.bind(this);
    this.triggerDelete = this.triggerDelete.bind(this);
  }

  handleToggleClick(event: {target: Element, preventDefault: () => void}) {
    // Hacky solution to prevent bubbling from `<a>` elements. Normally, we'd use stopPropagation.
    // However, the problem here is that we're injecting some html provided by the server, which
    // internally generates A elements. Therefore, we don't have such option, so we're doing it
    // here.
    if (event.target.tagName !== "A" && ! this.pending()) {
      event.preventDefault();
      this.props.onClick();
    }
  }

  isAnalystCreatedSelectable() {
    const kind = this.props.selectable.kind;
    return kind === "view" || kind === "analyst_table";
  }

  hasRenderableContent() {
    return this.props.filter.anyColumnMatches(this.props.selectable.columns);
  }

  editLinkUrl() {
    const selectable = this.props.selectable;
    return `${this.props.selectablesEditUrl}?kind=${selectable.kind}&id=${selectable.internal_id}`;
  }

  triggerDelete(event: {preventDefault: () => void}) {
    if (confirm(`Do you want to permanently delete ${this.props.selectable.id}?`)) { // eslint-disable-line no-alert
      this.props.channel.push("delete_selectable", {
        internal_id: this.props.selectable.internal_id,
        kind: this.props.selectable.kind,
      });
    }
    event.preventDefault();
  }

  renderSelectableActionMenu() {
    if (this.pending()) {
      return null;
    } else {
      return (
        <span className="pull-right">
          &nbsp;
          <a className="btn btn-xs btn-default" href={this.editLinkUrl()}>Edit</a>
          &nbsp;
          <a className="btn btn-xs btn-danger" onClick={this.triggerDelete}>Delete</a>
        </span>
      );
    }
  }

  broken() {
    if (this.props.selectable.broken) {
      return {
        title: VIEW_INVALID_MESSAGE,
        "data-toggle": "tooltip",
        className: "list-group-item-heading alert-danger",
      };
    } else {
      return {
        className: "list-group-item-heading",
      };
    }
  }

  pending() {
    return this.props.selectable.creation_status === "pending";
  }

  renderIcon() {
    if (this.pending()) {
      return <img src="/images/loader.gif" role="presentation" height="12" width="12" />;
    } else {
      const glyphType = this.props.expanded ? "glyphicon glyphicon-minus" : "glyphicon glyphicon-plus";
      return <span className={glyphType} />;
    }
  }

  renderSelectableView() {
    return (
      <div className="list-group-item">
        <div onClick={this.handleToggleClick} {...this.broken()}>
          {this.renderIcon()}
          &nbsp;
          {this.props.selectable.id}

          {this.isAnalystCreatedSelectable() ? this.renderSelectableActionMenu() : null}
        </div>

        {(() => {
          if (this.props.expanded) {
            return <ColumnsView columns={this.props.selectable.columns} filter={this.props.filter} />;
          } else {
            return null;
          }
        })()}
      </div>
    );
  }

  render() {
    if (this.hasRenderableContent()) {
      activateTooltips();
      return this.renderSelectableView();
    } else {
      return null;
    }
  }
}
