import React from "react";

import {Columns} from "./columns";

export const Table = (props) =>
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
        return <Columns columns={props.table.columns} />;
      } else {
        return null;
      }
    })()}
  </a>;

Table.propTypes = {
  table: React.PropTypes.shape({
    id: React.PropTypes.string.isRequired,
    columns: Columns.propTypes.columns,
  }),
  onClick: React.PropTypes.func,
  expanded: React.PropTypes.bool.isRequired,
};
