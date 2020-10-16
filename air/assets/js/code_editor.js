// @flow

import React from "react";
import { Controlled as Codemirror } from "react-codemirror2";
import type { Editor, EditorChange } from "codemirror";

import completions from "./code_editor/completion";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");
require("./code_editor/mode");

export type Annotations = Array<{
  start: { line: number, chr: number },
  end: { line: number, chr: number },
  properties: Object,
}>;

type Props = {
  onRun: () => void,
  onChange: (string) => void,
  tableNames: string[],
  columnNames: string[],
  statement: string,
  annotations: Annotations,
};

export default class CodeEditor extends React.Component<Props> {
  constructor(props: Props) {
    super(props);
    this.editor = null;
    window.insertWordInEditor = this.insertWordInEditor.bind(this);
    window.showErrorLocation = this.showErrorLocation.bind(this);
    window.clearErrorLocation = this.clearErrorLocation.bind(this);
  }

  editor: Editor;

  errorMarker: ?any;

  componentDidUpdate(prevProps: Props) {
    if (prevProps.annotations !== this.props.annotations && this.editor) {
      const doc = this.editor.getDoc();
      doc.getAllMarks().forEach((mark) => mark.clear());
      this.props.annotations.forEach((annotation) => {
        doc.markText(annotation.start, annotation.end, annotation.properties);
      });
    }
  }

  run = () => {
    const { onRun } = this.props;
    onRun();
  };

  showHint = (editor: Editor) => {
    editor.showHint({ hint: this.completionList });
  };

  onBeforeChange = (_editor: Editor, _data: EditorChange, value: string) => {
    this.props.onChange(value);
  };

  editorDidMount = (editor: Editor) => {
    this.editor = editor;
    editor.focus();
  };

  componentWillUnmount = () => {
    window.insertWordInEditor = null;
  };

  completionList = (cm: Editor) => {
    const { tableNames, columnNames, statement } = this.props;
    return completions(
      cm.getLine(cm.getCursor().line),
      cm.getCursor().ch,
      (pos) => ({ ...cm.getCursor(), ch: pos }),
      tableNames,
      columnNames,
      statement
    );
  };

  insertWordInEditor = (word: String) => {
    const doc = this.editor.getDoc();
    doc.replaceSelection(word);
    this.editor.focus();
  };

  clearErrorLocation = () => {
    if (this.errorMarker) {
      this.errorMarker.clear();
      this.errorMarker = null;
    }
  };

  showErrorLocation = (line: number, ch: number) => {
    this.clearErrorLocation();
    const doc = this.editor.getDoc();
    this.errorMarker = doc.markText(
      { line, ch },
      { line, ch: ch + 1 },
      { className: "error-location" }
    );
  };

  render = () => {
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
      autofocus: true,
      extraKeys: {
        "Ctrl-Enter": this.run,
        "Cmd-Enter": this.run,
        "Ctrl-Space": this.showHint,
        "Cmd-Space": this.showHint,
      },
    };

    const { statement } = this.props;
    return (
      <Codemirror
        value={statement}
        editorDidMount={this.editorDidMount}
        onBeforeChange={this.onBeforeChange}
        options={options}
        className="editable"
      />
    );
  };
}
