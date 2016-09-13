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
  "SHOW TABLES;",
  "LEFT JOIN", "LEFT INNER JOIN", "RIGHT INNER JOIN",
  "OUTER JOIN", "FULL OUTER JOIN", "LEFT OUTER JOIN", "RIGHT OUTER JOIN",
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
    this.editor = codeMirrorComponent.getCodeMirrorInstance();
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
    let end = cur.ch;

    // find end of the word
    while (end < curLine.length && regex.test(curLine.charAt(end))) {
      end++;
    }

    // We want to construct the longest possible match using the previous
    // SQL words the analyst has written. For example, if the analyst has
    // already written "LEFT INNER J", then we want to take this into account,
    // and only favour "LEFT INNER JOIN" as a match clause.
    // The current approach taken doesn't work well across lines, as we (if
    // we take the full document into account), lose the location info of
    // where the match starts, when prepping and creating the RegExp match string.
    // This could be worked around, but it seems only for marginal gains.
    const codeWords = curLine.slice(0, end).split(/\s/);
    const potentialMatchSequences = [];
    for (let i = codeWords.length; i >= 0; i--) {
      const wordsToUse = [];
      for (let j = i; j < codeWords.length; j++) {
        wordsToUse.push(codeWords[j]);
      }
      if (wordsToUse.length > 0) {
        const potentialMatchSequence = `(${wordsToUse.join(" ")})?`;
        potentialMatchSequences.push(potentialMatchSequence);
      }
    }
    const finalClause = _.chain(potentialMatchSequences).
      reverse().
      join("").
      value();

    const matcher = new RegExp(finalClause, "i");

    const sortOrder = (candidate) =>
      // We favour the longest matches over the more specific ones
      candidate.text.length * -1;

    const showColumnsFromTables =
      _.map(this.props.tableNames, tableName => `SHOW COLUMNS FROM ${tableName};`);

    const fromWithTables =
      _.map(this.props.tableNames, tableName => `FROM ${tableName};`);

    const list = _.chain(KEYWORDS).
    concat(this.props.tableNames).
    concat(showColumnsFromTables).
    concat(fromWithTables).
    concat(this.props.columnNames).
    map((candidate) => {
      const bestMatch = candidate.match(matcher).shift();
      if (bestMatch === "") {
        return null;
      } else {
        return {
          text: candidate,
          /* eslint-disable new-cap */
          from: this.editor.Pos(cur.line, end - bestMatch.length),
          to: this.editor.Pos(cur.line, end),
          /* eslint-enable new-cap */
        };
      }
    }).
    reject((candidate) => candidate === null).
    sortBy((item) => sortOrder(item)).
    value();

    return {
      list,
      from: 0,
      end,
    };
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
