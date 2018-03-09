// @flow

import React from "react";
import pagedown from "pagedown";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";
import {DebugExport} from "./debug_export";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

export const Error =
  (props: {id: string, statement: string, error: string, info: string[], debugModeEnabled: boolean}) =>
    <div className="panel panel-danger">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={props.statement} />

        <h4>Query failed</h4>
        <p dangerouslySetInnerHTML={mdToHtml(props.error)} />

        <Info info={props.info} />
        <DebugExport id={props.id} debugModeEnabled={props.debugModeEnabled} />
      </div>
    </div>;
