// @flow

import React from "react";
import {UnControlled as Codemirror} from "react-codemirror2";

require("codemirror/mode/sql/sql");

export const CodeViewer = (props: {statement: string}) => {
  const options = {
    indentUnit: 2,
    indentWithTabs: false,
    lineNumbers: true,
    lineWrapping: true,
    readOnly: true,
    mode: "text/x-pgsql",
    showCursorWhenSelecting: true,
    smartIndent: true,
    viewportMargin: Infinity,
    cursorBlinkRate: -1,
  };

  return (
    <Codemirror
      value={props.statement}
      options={options}
    />
  );
};
