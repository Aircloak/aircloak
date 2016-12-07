// @flow

import React from "react";

import {ColumnsView} from "./columns";
import type {Column} from "./columns";

export type Table = {
  id: string,
  columns: Column[],
  editLink: string
};

export const TableView = (props: {table: Table, onClick: () => void, expanded: boolean}) =>
  <div onClick={props.onClick} className="list-group-item">
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
        if (props.table.editLink) {
          return <a href={props.table.editLink} onClick={e => e.stopPropagation()}>{props.table.id}</a>;
        } else {
          return props.table.id;
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
