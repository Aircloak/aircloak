// @flow

import React from "react";
import {UnControlled as Codemirror} from "react-codemirror2";

require("./code_editor/mode");

export const CodeViewer = (props: {statement: string}) => {
  const options = {
    indentUnit: 2,
    indentWithTabs: false,
    lineNumbers: false,
    lineWrapping: true,
    readOnly: true,
    mode: "text/x-aircloak-sql",
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
