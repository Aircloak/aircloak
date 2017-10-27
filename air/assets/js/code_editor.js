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
    window.insertWordInEditor = this.insertWordInEditor.bind(this);
    window.showErrorLocation = this.showErrorLocation.bind(this);
  }

  props: Props;
  setupComponent: () => void;
  reactCodeMirrorComponent: Codemirror;
  codeMirrorClass: () => Codemirror;
  completionList: () => void;
  insertWordInEditor: () => void;
  showErrorLocation: () => void;
  errorMarker: null;

  setupComponent(codeMirrorComponent: {getCodeMirrorInstance: () => Codemirror}) {
    // This appears to happen when the component disappears
    if (codeMirrorComponent === null) return;

    const codeMirrorClass = codeMirrorComponent.getCodeMirrorInstance();
    codeMirrorClass.commands.run = (_cm) => {
      this.props.onRun();
    };
    codeMirrorClass.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList});
    };

    this.reactCodeMirrorComponent = codeMirrorComponent;
    this.codeMirrorClass = codeMirrorClass;
  }

  completionList(cm: Codemirror) {
    return completions(
      cm.getLine(cm.getCursor().line),
      cm.getCursor().ch,
      /* eslint-disable new-cap */
      (pos) => this.codeMirrorClass.Pos(cm.getCursor().line, pos),
      /* eslint-enable new-cap */
      this.props.tableNames,
      this.props.columnNames,
      this.props.statement,
    );
  }

  insertWordInEditor(word: String) {
    const editor = this.reactCodeMirrorComponent.getCodeMirror();
    const doc = editor.getDoc();
    doc.replaceSelection(word);
    editor.focus();
  }

  showErrorLocation(line: number, ch: number) {
    if (this.errorMarker) {
      this.errorMarker.clear();
      this.errorMarker = null;
    }
    if (line < 0 || ch < 0) return;
    const editor = this.reactCodeMirrorComponent.getCodeMirror();
    const doc = editor.getDoc();
    this.errorMarker = doc.markText({line, ch}, {line, ch: ch + 1}, {className: "error-location"});
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
        "Cmd-Space": "autoComplete",
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
