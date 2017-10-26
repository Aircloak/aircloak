// @flow

import React from "react";

import type {GraphInfoT} from "./graph_data";
import {GraphConfig} from "./graph_data";

type ColumnAction = (columnIndex: number) => () => void;

type Props = {
  graphInfo: GraphInfoT,
  graphConfig: GraphConfig,
  addX: ColumnAction,
  addY: ColumnAction,
  remove: ColumnAction
};

const activeClass = (active) => (active ? "btn btn-info" : "btn btn-default");

const noneClass = (graphConfig, column) =>
  activeClass(!graphConfig.xColumns().includes(column) && !graphConfig.yColumns().includes(column));

const xClass = (graphConfig, column) => activeClass(graphConfig.xColumns().includes(column));

const yClass = (graphConfig, column) => activeClass(graphConfig.yColumns().includes(column));

export const GraphConfigView = (props: Props) =>
  <form className="form-horizontal">
    {props.graphInfo.xColumns().map((column, columnIndex) =>
      <div key={columnIndex} className="form-group">
        <label className="col-sm-3 control-label">{column}</label>
        <div className="col-sm-9 btn-group" role="group">
          <button
            type="button"
            className={noneClass(props.graphConfig, columnIndex)}
            onClick={props.remove(columnIndex)}
          > None </button>

          <button
            type="button"
            className={xClass(props.graphConfig, columnIndex)}
            onClick={props.addX(columnIndex)}
          >X</button>

          <button
            type="button"
            className={yClass(props.graphConfig, columnIndex)}
            onClick={props.addY(columnIndex)}
            disabled={!props.graphInfo.usableAsY(columnIndex)}
          > Y </button>
        </div>
      </div>
    )}
  </form>;
