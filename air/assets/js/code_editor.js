// @flow

import type { Node } from "React";
import React from "react";
import { Controlled as Codemirror } from "react-codemirror2";
import type { Editor, EditorChange, Position } from "codemirror";

import completions from "./code_editor/completion";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");
require("./code_editor/mode");

export type Annotations =
  | Array<{
      range: {
        start: Position,
        end: Position,
      },
      query_type: string,
      emulated: boolean,
      anonymization_type: string,
      noise_layers: number,
    }>
  | {|
      type: "ParseError" | "CompilationError",
      message: string,
      location: Position | null,
    |}
  | "loading";

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

  editor: ?Editor;

  errorMarker: ?any;

  componentDidUpdate(prevProps: Props) {
    if (prevProps.annotations !== this.props.annotations && this.editor) {
      this.updateAnnotations(this.editor, this.props.annotations);
      this.setState({});
    }
  }

  updateAnnotations: (editor: Editor, annotations: Annotations) => void = (
    editor,
    annotations
  ) => {
    const doc = editor.getDoc();
    doc.getAllMarks().forEach((mark) => mark.clear());
    if (Array.isArray(annotations)) {
      annotations.forEach(({ range, ...data }) => {
        doc.markText(range.start, range.end, {
          clearWhenEmpty: true,
          data,
          css: `background-color: ${
            data.query_type === "standard"
              ? "transparent"
              : data.query_type === "restricted"
              ? "rgba(200, 20, 20, 0.05)"
              : "rgba(205, 205, 250, 0.25)"
          }`,
        });
      });
    } else if (typeof annotations == "object" && annotations.location != null) {
      this.showErrorLocation(
        annotations.location.line,
        annotations.location.ch
      );
    }
  };

  run: () => void = () => {
    const { onRun } = this.props;
    onRun();
  };

  showHint: (editor: Editor) => void = (editor: Editor) => {
    editor.showHint({ hint: this.completionList });
  };

  onBeforeChange: (_editor: any, _data: any, value: string) => void = (
    _editor: Editor,
    _data: EditorChange,
    value: string
  ) => {
    this.props.onChange(value);
  };

  editorDidMount: (editor: Editor) => void = (editor: Editor) => {
    this.editor = editor;
    this.updateAnnotations(editor, this.props.annotations);
    editor.focus();
  };

  componentWillUnmount: () => void = () => {
    window.insertWordInEditor = null;
  };

  completionList: (cm: Editor) => {| from: any, list: any, to: any |} = (
    cm: Editor
  ) => {
    const { tableNames, columnNames, statement } = this.props;
    return completions(
      cm.getDoc().getLine(cm.getCursor().line),
      cm.getCursor().ch,
      (pos) => ({ ...cm.getCursor(), ch: pos }),
      tableNames,
      columnNames,
      statement
    );
  };

  insertWordInEditor: (word: string) => void = (word) => {
    const editor = this.editor;
    if (editor != null) {
      const doc = editor.getDoc();
      doc.replaceSelection(word);
      editor.focus();
    }
  };

  clearErrorLocation: () => void = () => {
    if (this.errorMarker) {
      this.errorMarker.clear();
      this.errorMarker = null;
    }
  };

  showErrorLocation: (line: number, ch: number) => void = (
    line: number,
    ch: number
  ) => {
    this.clearErrorLocation();
    const editor = this.editor;
    if (editor != null) {
      const doc = editor.getDoc();
      this.errorMarker = doc.markText(
        { line, ch },
        { line, ch: ch + 1 },
        { className: "error-location" }
      );
    }
  };

  onCursorActivity: (editor: Editor) => void = (editor) => {
    this.setState({}); // force rerender
  };

  renderStatusBar: (Annotations) => Node = (annotations) => {
    if (annotations === "loading") {
      return <div className="status-bar status-bar-loading">Loading</div>;
    } else if (Array.isArray(annotations)) {
      const editor = this.editor;
      if (editor) {
        const marks = editor.getDoc().findMarksAt(editor.getCursor());
        if (marks.length > 0) {
          const last = marks[marks.length - 1];
          if (last["data"] != null) {
            const data = last.data;
            return (
              <div
                className="status-bar"
                style={{
                  background:
                    data.query_type === "standard"
                      ? "transparent"
                      : data.query_type === "restricted"
                      ? "rgb(252 243 244)"
                      : "rgb(242 242 254)",
                }}
              >
                {data.emulated ? "Emulated" : "Native"}{" "}
                <span>{data.query_type}</span> query using{" "}
                {data.anonymization_type} based anonymization.
              </div>
            );
          }
        }
        return null;
      }
      return null;
    } else {
      return (
        <div className="status-bar status-bar-error">{annotations.message}</div>
      );
    }
  };

  render: () => Node = () => {
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

    const { statement, annotations } = this.props;

    return (
      <div className="flex-grow-1 editor-container">
        <Codemirror
          value={statement}
          editorDidMount={this.editorDidMount}
          onBeforeChange={this.onBeforeChange}
          onCursorActivity={this.onCursorActivity}
          options={options}
          className="editable"
        />
        {this.renderStatusBar(annotations)}
      </div>
    );
  };
}
