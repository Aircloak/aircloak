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
  expanded: boolean
};

export const SelectableView = (props: Props) =>
  <div
    className="list-group-item"
    onClick={(event) => {
      // Hacky solution to prevent bubbling from `<a>` elements. Normally, we'd use stopPropagation.
      // However, the problem here is that we're injecting some html provided by the server, which
      // internally generates A elements. Therefore, we don't have such option, so we're doing it
      // here.
      if (event.target.tagName !== "A") {
        event.preventDefault();
        props.onClick();
      }
    }}
  >
    <div className="list-group-item-heading">
      {(() => {
        if (props.expanded) {
          return <span className="glyphicon glyphicon-minus"></span>;
        } else {
          return <span className="glyphicon glyphicon-plus"></span>;
        }
      })()}

      &nbsp;

      {props.selectable.id}

      {(() => {
        if (!props.readOnly && props.selectable.edit_link && props.selectable.delete_html) {
          return (
            <span className="pull-right">
              <a className="btn btn-xs" href={props.selectable.edit_link}>Edit</a>
              &nbsp;
              <span dangerouslySetInnerHTML={{__html: props.selectable.delete_html}}/>
            </span>
          );
        } else {
          return null;
        }
      })()}
    </div>

    {(() => {
      if (props.expanded) {
        return <ColumnsView columns={props.selectable.columns} />;
      } else {
        return null;
      }
    })()}
  </div>;
