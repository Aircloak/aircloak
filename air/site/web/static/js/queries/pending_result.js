import React from "react";

import {CodeViewer} from "../code_viewer";

export const PendingResult = (props) =>
  <div className="panel panel-info">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.statement} />

      <p>
        <img src="/images/loader.gif" role="presentation" /> loading results
      </p>
    </div>
  </div>;

PendingResult.propTypes = {
  statement: React.PropTypes.string,
};
