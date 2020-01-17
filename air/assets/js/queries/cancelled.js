// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {DebugExport} from "./debug_export";
import {ShareButton} from "./share_button";

import type {CancelledResult} from "./result";

export const Cancelled = (props: {result: CancelledResult, debugModeEnabled: boolean}) =>
  <div className="panel panel-warning">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.result.statement} />
      <h4>Query cancelled</h4>
      <div className="options-menu">
        <ShareButton result={props.result} />
        <DebugExport id={props.result.id} debugModeEnabled={props.debugModeEnabled} />
      </div>
    </div>
  </div>;
