import React from "react";
import Codemirror from "react-codemirror";
import $ from "jquery";

import completions from "./code_editor/completion";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");

export class CodeEditor extends React.Component {
  constructor(props) {
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

  setupComponent(codeMirrorComponent) {
    this.editor = codeMirrorComponent.getCodeMirrorInstance();
    this.editor.commands.run = (_cm) => {
      this.props.onRun();
    };
    this.editor.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList});
    };
  }

  completionList(cm) {
    return completions(cm, this.editor, this.props.tableNames, this.props.columnNames);
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

CodeEditor.propTypes = {
  onRun: React.PropTypes.func.isRequired,
  onChange: React.PropTypes.func.isRequired,
  tableNames: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  columnNames: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  statement: React.PropTypes.string,
};
