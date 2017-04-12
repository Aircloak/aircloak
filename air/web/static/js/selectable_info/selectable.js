// @flow

import React from "react";
import $ from "jquery";

import {ColumnsView} from "./columns";
import {Filter} from "./filter";
import type {Column} from "./columns";

export type Selectable = {
  id: string,
  columns: Column[],
  edit_link: string,
  delete_html: string,
  broken: boolean,
};

type Props = {
  selectable: Selectable,
  onClick: () => void,
  expanded: boolean,
  filter: Filter,
};

const VIEW_INVALID_MESSAGE =
  "This view is no longer valid. The underlying data source or another view might have changed to cause this.";

export class SelectableView extends React.Component {
  handleToggleClick: () => void;
  isDatabaseView: () => boolean;
  renderDatabaseViewMenu: () => void;
  renderSelectableView: () => void;
  hasRenderableContent: () => boolean;

  constructor(props: Props) {
    super(props);

    this.handleToggleClick = this.handleToggleClick.bind(this);
    this.isDatabaseView = this.isDatabaseView.bind(this);
    this.hasRenderableContent = this.hasRenderableContent.bind(this);
    this.renderDatabaseViewMenu = this.renderDatabaseViewMenu.bind(this);
    this.renderSelectableView = this.renderSelectableView.bind(this);
  }

  handleToggleClick(event: {target: Element, preventDefault: () => void}) {
    // Hacky solution to prevent bubbling from `<a>` elements. Normally, we'd use stopPropagation.
    // However, the problem here is that we're injecting some html provided by the server, which
    // internally generates A elements. Therefore, we don't have such option, so we're doing it
    // here.
    if (event.target.tagName !== "A") {
      event.preventDefault();
      this.props.onClick();
    }
  }

  isDatabaseView() {
    return this.props.selectable.edit_link && this.props.selectable.delete_html;
  }

  hasRenderableContent() {
    return this.props.filter.anyColumnMatches(this.props.selectable.columns);
  }

  renderDatabaseViewMenu() {
    return (
      <span className="pull-right">
        &nbsp;
        <a className="btn btn-xs btn-default" href={this.props.selectable.edit_link}>Edit</a>
        &nbsp;
        <span
          dangerouslySetInnerHTML={{__html: this.props.selectable.delete_html}}
          onClick={(event) => event.preventDefault()}
        />
      </span>
    );
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

  renderSelectableView() {
    const glyphType = this.props.expanded ? "glyphicon glyphicon-minus" : "glyphicon glyphicon-plus";
    return (
      <div className="list-group-item">
        <div onClick={this.handleToggleClick} {...this.broken()}>
          <span className={glyphType} />
          &nbsp;
          {this.props.selectable.id}

          {this.isDatabaseView() ? this.renderDatabaseViewMenu() : null}
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
      setTimeout(() => $("[data-toggle='tooltip']").tooltip(), 0);
      return this.renderSelectableView();
    } else {
      return null;
    }
  }
}
