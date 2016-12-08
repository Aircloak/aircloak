// @flow

import React from "react";

import {ColumnsView} from "./columns";
import type {Column} from "./columns";

export type Table = {
  id: string,
  columns: Column[],
  edit_link: string,
  delete_html: string
};

export const TableView = (props: {table: Table, onClick: () => void, expanded: boolean}) =>
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

      {(() => {
        if (props.table.edit_link) {
          return <a href={props.table.edit_link}>{props.table.id}</a>;
        } else {
          return props.table.id;
        }
      })()}

      {(() => {
        if (props.table.delete_html) {
          return (
            <span className="pull-right" dangerouslySetInnerHTML={{__html: props.table.delete_html}} />
          );
        } else {
          return null;
        }
      })()}
    </div>

    {(() => {
      if (props.expanded) {
        return <ColumnsView columns={props.table.columns} />;
      } else {
        return null;
      }
    })()}
  </div>;
