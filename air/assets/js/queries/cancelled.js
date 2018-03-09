// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";

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

export const Cancelled = (props: {id: string, statement: string, debugModeEnabled: boolean}) =>
  <div className="panel panel-warning">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />
      <h4>Query cancelled</h4>
      {renderDebugExport(props)}
    </div>
  </div>;
