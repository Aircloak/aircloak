// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {DebugExport} from "./debug_export";
import {ShareButton} from "./share_button";

import type {Result} from "./result";

export const Cancelled = (props: {result: Result, debugModeEnabled: boolean, shareButton: boolean}) =>
  <div className="panel panel-warning">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.result.statement} />
      <h4>Query cancelled</h4>
      <div className="options-menu">
        <ShareButton result={props.result} enabled={props.shareButton} />
        <DebugExport id={props.result.id} debugModeEnabled={props.debugModeEnabled} />
      </div>
    </div>
  </div>;
