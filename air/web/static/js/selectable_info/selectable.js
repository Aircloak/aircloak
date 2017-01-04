// @flow

import React from "react";

import {ColumnsView} from "./columns";
import type {Column} from "./columns";

export type Selectable = {
  id: string,
  columns: Column[],
  edit_link: string,
  delete_html: string
};

type Props = {
  readOnly: boolean,
  selectable: Selectable,
  onClick: () => void,
  expanded: boolean,
  filterText: string,
};

export class SelectableView extends React.Component {
  handleToggleClick: () => void;
  isView: () => boolean;
  renderMenuItems: () => void;

  constructor(props: Props) {
    super(props);

    this.handleToggleClick = this.handleToggleClick.bind(this);
    this.isView = this.isView.bind(this);
    this.renderMenuItems = this.renderMenuItems.bind(this);
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

  isView() {
    return !this.props.readOnly &&
      this.props.selectable.edit_link &&
      this.props.selectable.delete_html;
  }

  renderMenuItems() {
    if (this.isView()) {
      return (
        <span className="pull-right">
          <a className="btn btn-xs btn-default" href={this.props.selectable.edit_link}>Edit</a>
          &nbsp;
          <span
            dangerouslySetInnerHTML={{__html: this.props.selectable.delete_html}}
            onClick={(event) => event.preventDefault()}
          />
        </span>
      );
    } else {
      return null;
    }
  }

  render() {
    const glyphType = this.props.expanded ? "glyphicon glyphicon-minus" : "glyphicon glyphicon-plus";
    return (
      <div className="list-group-item">
        <div onClick={this.handleToggleClick} className="list-group-item-heading">
          <span className={glyphType} />
          &nbsp;
          {this.props.selectable.id}
          {this.renderMenuItems()}
        </div>

        {(() => {
          if (this.props.expanded) {
            return <ColumnsView columns={this.props.selectable.columns} />;
          } else {
            return null;
          }
        })()}
      </div>
    );
  }
}
