// @flow

import React from "react";

import {ColumnsView} from "./columns";
import type {Column} from "./columns";

type Table = {
  id: string,
  columns: Column[],
};

export const TableView = (props: {table: Table, onClick: () => void, expanded: boolean}) =>
  <a href="#" onClick={props.onClick} className="list-group-item">
    <div className="list-group-item-heading">
      {(() => {
        if (props.expanded) {
          return <span className="glyphicon glyphicon-minus"></span>;
        } else {
          return <span className="glyphicon glyphicon-plus"></span>;
        }
      })()}

      &nbsp;

      {props.table.id}
    </div>

    {(() => {
      if (props.expanded) {
        return <ColumnsView columns={props.table.columns} />;
      } else {
        return null;
      }
    })()}
  </a>;
