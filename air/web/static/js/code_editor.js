// @flow

import React from "react";
import Codemirror from "react-codemirror";
import $ from "jquery";

import completions from "./code_editor/completion";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");

type Props = {
  onRun: () => void;
  onChange: () => void;
  tableNames: string[],
  columnNames: string[],
  statement: string,
}

export class CodeEditor extends React.Component {
  constructor(props: Props) {
    super(props);
    this.setupComponent = this.setupComponent.bind(this);
    this.completionList = this.completionList.bind(this);

    // The CodeMirror editor instance.
    // We need to keep track of it in order to provide
    // proper code hints, etc.
    // The variable is initialized in a callback from
    // the react component.
    this.editor = undefined;
  }

  props: Props;
  setupComponent: () => void;
  completionList: () => void;
  editor: Codemirror;

  setupComponent(codeMirrorComponent: {getCodeMirrorInstance: () => Codemirror}) {
    this.editor = codeMirrorComponent.getCodeMirrorInstance();
    this.editor.commands.run = (_cm) => {
      this.props.onRun();
    };
    this.editor.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList});
    };
  }

  completionList(cm: Codemirror) {
    return completions(
      cm.getLine(cm.getCursor().line),
      cm.getCursor().ch,
      /* eslint-disable new-cap */
      (pos) => this.editor.Pos(cm.getCursor().line, pos),
      /* eslint-enable new-cap */
      this.props.tableNames,
      this.props.columnNames
    );
  }

  render() {
    const options = {
      indentUnit: 2,
      indentWithTabs: false,
      lineNumbers: true,
      lineWrapping: true,
      matchBrackets: true,
      readOnly: false,
      mode: "text/x-pgsql",
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,
      cursorBlinkRate: 530,
    };

    $.extend(options, {
      autofocus: true,
      extraKeys: {
        "Ctrl-Enter": "run",
        "Cmd-Enter": "run",
        "Ctrl-Space": "autoComplete",
      },
    });

    return (
      <Codemirror
        ref={this.setupComponent}
        value={this.props.statement}
        onChange={this.props.onChange}
        options={options}
      />
    );
  }
}
