// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";

export const PendingResult = ({statement}: {statement: string}) =>
  <div className="panel panel-info">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={statement} />

      <p className="spinner">
        <img src="/images/loader.gif" role="presentation" /> loading results
      </p>
    </div>
  </div>;
