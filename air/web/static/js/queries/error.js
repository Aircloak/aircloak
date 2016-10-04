// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";

export const Error = (props: {statement: string, error: string, info: string[]}) =>
  <div className="panel panel-danger">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />

      <h4>Query failed</h4>
      <p>{props.error}</p>

      <Info info={props.info} />
    </div>
  </div>;
