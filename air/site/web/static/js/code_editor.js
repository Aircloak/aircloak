import React from "react";
import Codemirror from "react-codemirror";
import $ from "jquery";
import _ from "lodash";

require("codemirror/mode/sql/sql");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");

const KEYWORDS = [
  "SELECT", "FROM",
  "COUNT(*)", "SUM()", "MIN()", "MAX()", "AVG()", "STDDEV()", "MEDIAN()",
  "SHOW TABLES", "SHOW COLUMNS FROM",
  "LEFT INNER JOIN", "RIGHT INNER JOIN", "FULL OUTER JOIN", "LEFT OUTER JOIN", "RIGHT OUTER JOIN",
  "WHERE", "AND",
  "GROUP BY", "ORDER BY",
  "ASC", "DESC", "NOT",
  "IS NULL", "IS NOT NULL",
  "LIKE ''", "ILIKE ''", "NOT LIKE ''", "NOT ILIKE ''",
  "IN ()", "NOT IN ()",
];

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
    // Skip setup for read-only
    if (this.props.readOnly) {
      return;
    }

    this.editor = codeMirrorComponent.getCodeMirrorInstance();
    this.editor.commands.save = (_cm) => {
      this.props.onSave();
    };
    this.editor.commands.run = (_cm) => {
      this.props.onRun();
    };
    this.editor.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList});
    };
  }

  completionList(cm) {
    const regex = /(\w|\.)/;
    const cur = cm.getCursor();
    const curLine = cm.getLine(cur.line);
    let start = cur.ch;
    let end = start;

    // find start and end of the word
    while (end < curLine.length && regex.test(curLine.charAt(end))) {
      end++;
    }
    while (start > 0 && regex.test(curLine.charAt(start - 1))) {
      start--;
    }

    const curWord = curLine.slice(start, end);
    const fuzzyMatcher = new RegExp(curWord.replace(/(.)/g, "$1.*"), "i");

    const sortOrder = (text) => {
      // Place items that start with the given word at the top of the completion list.
      if (text.startsWith(curWord)) {
        return `0${text}`;
      } else {
        return `1${text}`;
      }
    };

    // TODO: auto-complete column and table names
    const list = _.chain(KEYWORDS).
    filter((candidate) => candidate.match(fuzzyMatcher)).
    sortBy((item) => sortOrder(item)).
    value();

    return {
      list,
      /* eslint-disable new-cap */
      from: this.editor.Pos(cur.line, start),
      to: this.editor.Pos(cur.line, end),
      /* eslint-enable new-cap */
    };
  }

  render() {
    const options = {
      indentUnit: 2,
      indentWithTabs: false,
      lineNumbers: true,
      lineWrapping: true,
      matchBrackets: true,
      readOnly: this.props.readOnly,
      mode: "text/x-pgsql",
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,
      cursorBlinkRate: (this.props.readOnly ? -1 : 530),
    };

    if (! this.props.readOnly) {
      $.extend(options, {
        autofocus: true,
        extraKeys: {
          "Ctrl-Enter": "run",
          "Cmd-Enter": "run",
          "Ctrl-Space": "autoComplete",
        }});
    }

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
  onSave: React.PropTypes.func.isRequired,
  onRun: React.PropTypes.func.isRequired,
  onChange: React.PropTypes.func.isRequired,
  readOnly: React.PropTypes.bool,
  completions: React.PropTypes.arrayOf(React.PropTypes.shape({
    displayText: React.PropTypes.string,
    text: React.PropTypes.string,
  })),
  statement: React.PropTypes.string,
};
