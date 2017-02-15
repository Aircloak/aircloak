// @flow

import React from "react";

type ColumnAction = (column:string) => () => void;

type Props = {
  graphInfo: GraphInfoT,
  graphConfig: GraphConfigT,
  addX: ColumnAction,
  addY: ColumnAction,
  remove: ColumnAction
};

const noneClass = (graphConfig, column) =>
  !graphConfig.xColumns().includes(column) && !graphConfig.yColumns().includes(column) ?
    "btn btn-info" : "btn btn-default"

const xClass = (graphConfig, column) => graphConfig.xColumns().includes(column) ? "btn btn-info" : "btn btn-default"

const yClass = (graphConfig, column) => graphConfig.yColumns().includes(column) ? "btn btn-info" : "btn btn-default"

export const GraphConfigView = (props: Props) =>
  <form className="form-horizontal">
    {props.graphInfo.xColumns().map((column, i) => {
      return (
        <div key={i} className="form-group">
          <label className="col-sm-3 control-label">{column}</label>
          <div className="col-sm-9 btn-group" role="group">
            <button
              type="button"
              className={noneClass(props.graphConfig, column)}
              onClick={props.remove(column)}
            > None </button>

            <button type="button" className={xClass(props.graphConfig, column)} onClick={props.addX(column)}>X</button>

            <button
              type="button"
              className={yClass(props.graphConfig, column)}
              onClick={props.addY(column)}
              disabled={!props.graphInfo.usableAsY(column)}
            > Y </button>
          </div>
        </div>
      )
    })}
  </form>
