// @flow

import React from "react";

import marked from "marked";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";

const mdToHtml = (text: string) => ({__html: marked(text)});

export const Error = (props: {statement: string, error: string, info: string[]}) =>
  <div className="panel panel-danger">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />

      <h4>Query failed</h4>
      <p dangerouslySetInnerHTML={mdToHtml(props.error)} />

      <Info info={props.info} />
    </div>
  </div>;
