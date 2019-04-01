// @flow

import React from "react";
import {UnControlled as Codemirror} from "react-codemirror2";
import $ from "jquery";
import _ from "lodash";

import completions from "./code_editor/completion";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");
require("./code_editor/mode");

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
    this.completionList = this.completionList.bind(this);
    this.run = this.run.bind(this);
    this.showHint = this.showHint.bind(this);
    window.insertWordInEditor = this.insertWordInEditor.bind(this);
    window.showErrorLocation = this.showErrorLocation.bind(this);
    window.clearErrorLocation = this.clearErrorLocation.bind(this);
  }

  props: Props;
  reactCodeMirrorComponent: Codemirror;
  codeMirrorClass: () => Codemirror;
  completionList: () => void;
  insertWordInEditor: () => void;
  showErrorLocation: () => void;
  clearErrorLocation: () => void;
  errorMarker: null;

  run() {
    this.props.onRun();
  }

  showHint(editor) {
    editor.showHint({hint: this.completionList});
  }

  completionList(cm: Codemirror) {
    return completions(
      cm.getLine(cm.getCursor().line),
      cm.getCursor().ch,
      (pos) => _.merge({}, cm.getCursor(), {ch: pos}),
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

  clearErrorLocation() {
    if (this.errorMarker) {
      this.errorMarker.clear();
      this.errorMarker = null;
    }
  }

  showErrorLocation(line: number, ch: number) {
    this.clearErrorLocation();
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
      mode: "text/x-aircloak-sql",
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,
      cursorBlinkRate: 530,
    };

    $.extend(options, {
      autofocus: true,
      extraKeys: {
        "Ctrl-Enter": this.run,
        "Cmd-Enter": this.run,
        "Ctrl-Space": this.showHint,
        "Cmd-Space": this.showHint
      },
    });

    return (
      <Codemirror
        value={this.props.statement}
        onChange={this.props.onChange}
        options={options}
      />
    );
  }
}
