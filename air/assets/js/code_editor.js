// @flow

import type { Node } from "React";
import React from "react";
import { Controlled as Codemirror } from "react-codemirror2";
import type { Editor, EditorChange, Position, TextMarker } from "codemirror";

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

type State = {
  activeMark: TextMarker | null,
};

const isBetween = (start: Position, end: Position, pos: Position) => {
  if (start.line === end.line && pos.line === start.line) {
    return pos.ch >= start.ch && pos.ch <= end.ch;
  } else if (start.line === pos.line) {
    return pos.ch >= start.ch;
  } else if (end.line === pos.line) {
    return pos.ch <= end.ch;
  } else if (pos.line > start.line && pos.line < end.line) {
    return true;
  }
  return false;
};

export default class CodeEditor extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.editor = null;
    window.insertWordInEditor = this.insertWordInEditor.bind(this);
    window.showErrorLocation = this.showErrorLocation.bind(this);
    window.clearErrorLocation = this.clearErrorLocation.bind(this);
    this.state = { activeMark: null };
  }

  editor: ?Editor;

  errorMarker: ?any;

  componentDidUpdate(prevProps: Props, prevState: State) {
    if (
      this.editor &&
      (prevProps.annotations !== this.props.annotations ||
        this.state.activeMark !== prevState.activeMark)
    ) {
      this.updateAnnotations(this.editor, this.props.annotations);
      this.setState({});
    }
  }

  getAnnotationColor: (string, boolean) => string = (
    queryType,
    highlighted
  ) => {
    switch (queryType) {
      case "standard":
        return highlighted ? "#fefff2" : "#fbfbfb";
      case "restricted":
        return highlighted ? "#f3e5e7" : "rgb(252 243 244)";
      default:
        return highlighted ? "#dedefb" : "rgb(242, 242, 254)";
    }
  };

  updateAnnotations: (editor: Editor, annotations: Annotations) => void = (
    editor,
    annotations
  ) => {
    const doc = editor.getDoc();
    doc.getAllMarks().forEach((mark) => mark.clear());
    if (Array.isArray(annotations)) {
      annotations.forEach(({ range, ...data }, i) => {
        doc.markText(range.start, range.end, {
          clearWhenEmpty: true,
          data,
          css: `background-color: ${this.getAnnotationColor(
            data.query_type,
            isBetween(range.start, range.end, editor.getCursor()) &&
              (i + 1 === annotations.length ||
                !isBetween(
                  annotations[i + 1].range.start,
                  annotations[i + 1].range.end,
                  editor.getCursor()
                ))
          )}`,
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
    this.onCursorActivity(editor);
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
    const marks = editor.getDoc().findMarksAt(editor.getCursor());
    if (marks.length > 0) {
      const last = marks[marks.length - 1];
      this.setState({ activeMark: last });
    } else {
      this.setState({ activeMark: null });
    }
  };

  renderStatusBar: (Annotations) => Node = (annotations) => {
    if (annotations === "loading") {
      return <div className="status-bar status-bar-loading">Loading</div>;
    } else if (Array.isArray(annotations)) {
      const { activeMark } = this.state;
      if (activeMark && activeMark.data) {
        const data = activeMark.data;
        switch (data.query_type) {
          case "standard":
            return (
              <div
                className="status-bar"
                style={{
                  background: this.getAnnotationColor("standard", true),
                }}
              >
                <span>Standard query{data.emulated ? " (emulated)" : ""}.</span>
                <span>No restrictions</span>
              </div>
            );
          case "restricted":
            return (
              <div
                className="status-bar"
                style={{
                  background: this.getAnnotationColor("restricted", true),
                }}
              >
                <span>
                  Restricted query{data.emulated ? " (emulated)" : ""}.
                </span>
                <a href="/docs/#/sql/restrictions" target="_blank">
                  <i className="fas fa-question-circle"></i> Restrictions apply
                </a>
              </div>
            );
          default:
            return (
              <div
                className="status-bar"
                style={{
                  background: this.getAnnotationColor("anonymized", true),
                }}
              >
                <span>
                  Anonymized query using {data.anonymization_type} based
                  anonymization.
                </span>
                <a href="/docs/#/sql/restrictions" target="_blank">
                  <i className="fas fa-question-circle"></i> Restrictions apply
                </a>
              </div>
            );
        }
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
