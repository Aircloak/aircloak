// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {DebugExport} from "./debug_export";

export const Cancelled = (props: {id: string, statement: string, debugModeEnabled: boolean}) =>
  <div className="panel panel-warning">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />
      <h4>Query cancelled</h4>
      <DebugExport id={props.id} debugModeEnabled={props.debugModeEnabled} />
    </div>
  </div>;
