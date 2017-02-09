// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";

export type Error = {
  human_description: string,
}

export const ErrorView = (props: {statement: string, error: Error, info: string[]}) =>
  <div className="panel panel-danger">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />

      <h4>Query failed</h4>
      <p>{props.error.human_description}</p>

      <Info info={props.info} />
    </div>
  </div>;
