// @flow

import React from "react";

import type { GraphInfoT } from "./graph_data";
import { GraphConfig } from "./graph_data";

type ColumnAction = (columnIndex: number) => () => void;

type Props = {
  graphInfo: GraphInfoT,
  graphConfig: GraphConfig,
  addX: ColumnAction,
  addY: ColumnAction,
  remove: ColumnAction
};

const activeClass = active => (active ? "btn btn-info" : "btn btn-default");

const noneClass = (graphConfig, column) =>
  activeClass(
    !graphConfig.xColumns().includes(column) &&
      !graphConfig.yColumns().includes(column)
  );

const xClass = (graphConfig, column) =>
  activeClass(graphConfig.xColumns().includes(column));

const yClass = (graphConfig, column) =>
  activeClass(graphConfig.yColumns().includes(column));

export default ({ graphInfo, graphConfig, remove, addX, addY }: Props) => {
  return (
    <form className="form-horizontal">
      {graphInfo.xColumns().map((column, columnIndex) => (
        <div key={column} className="form-group">
          <span className="col-sm-3 control-label">{column}</span>
          <div className="col-sm-9 btn-group" role="group">
            <button
              type="button"
              className={noneClass(graphConfig, columnIndex)}
              onClick={remove(columnIndex)}
            >
              None
            </button>

            <button
              type="button"
              className={xClass(graphConfig, columnIndex)}
              onClick={addX(columnIndex)}
            >
              X
            </button>

            <button
              type="button"
              className={yClass(graphConfig, columnIndex)}
              onClick={addY(columnIndex)}
              disabled={!graphInfo.usableAsY(columnIndex)}
            >
              Y
            </button>
          </div>
        </div>
      ))}
    </form>
  );
};
