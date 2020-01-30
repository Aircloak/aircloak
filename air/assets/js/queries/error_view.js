// @flow
/* eslint-disable react/no-danger */

import React from "react";
import pagedown from "pagedown";

import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";

import type { ErrorResult } from "./result";

const mdToHtml = (text: string) => ({
  __html: pagedown.getSanitizingConverter().makeHtml(text)
});

export default ({
  result,
  debugModeEnabled
}: {
  result: ErrorResult,
  debugModeEnabled: boolean
}) => {
  return (
    <div className="panel panel-danger">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={result.statement} />

        <h4>Query failed</h4>
        <p dangerouslySetInnerHTML={mdToHtml(result.error)} />

        <InfoView info={result.info} />

        <div className="options-menu">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>
      </div>
    </div>
  );
};
