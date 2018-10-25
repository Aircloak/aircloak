// @flow

import React from "react";
import pagedown from "pagedown";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";
import {DebugExport} from "./debug_export";
import {ShareButton} from "./share_button";

import type {Result} from "./result";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

export const Error =
  (props: {result: Result, debugModeEnabled: boolean}) =>
    <div className="panel panel-danger">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={props.result.statement} />

        <h4>Query failed</h4>
        <p dangerouslySetInnerHTML={mdToHtml(props.result.error)} />

        <Info info={props.result.info} />

        <div className="options-menu">
          <ShareButton result={props.result} />
          <DebugExport id={props.result.id} debugModeEnabled={props.debugModeEnabled} />
        </div>
      </div>
    </div>;
