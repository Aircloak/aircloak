// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";

export const Cancelled = (props: {statement: string}) =>
  <div className="panel panel-warning">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />
      <h4>Query cancelled</h4>
    </div>
  </div>;
