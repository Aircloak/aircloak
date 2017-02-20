// @flow

import React from "react";

import type {GraphInfoT} from "./graph_data";
import {GraphConfig} from "./graph_data";

type ColumnAction = (column: string) => () => void;

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
    {props.graphInfo.xColumns().map((column, i) =>
      <div key={i} className="form-group">
        <label className="col-sm-3 control-label">{column}</label>
        <div className="col-sm-9 btn-group" role="group">
          <button
            type="button"
            className={noneClass(props.graphConfig, column)}
            onClick={props.remove(column)}
          > None </button>

          <button
            type="button"
            className={xClass(props.graphConfig, column)}
            onClick={props.addX(column)}
          >X</button>

          <button
            type="button"
            className={yClass(props.graphConfig, column)}
            onClick={props.addY(column)}
            disabled={!props.graphInfo.usableAsY(column)}
          > Y </button>
        </div>
      </div>
    )}
  </form>;
