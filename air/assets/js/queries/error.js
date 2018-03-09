// @flow

import React from "react";
import pagedown from "pagedown";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

const renderDebugExport = (props: {id: string, debugModeEnabled: boolean}) => {
  if (props.debugModeEnabled) {
    return (
      <a className="btn btn-default btn-xs" href={`/queries/${props.id}/debug_export`}>
        Download debug export
      </a>
    );
  } else {
    return null;
  }
};

export const Error =
  (props: {id: string, statement: string, error: string, info: string[], debugModeEnabled: boolean}) =>
    <div className="panel panel-danger">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={props.statement} />

        <h4>Query failed</h4>
        <p dangerouslySetInnerHTML={mdToHtml(props.error)} />

        <Info info={props.info} />
        {renderDebugExport(props)}
      </div>
    </div>;
