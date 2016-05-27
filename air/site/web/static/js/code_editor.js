import React from "react";
import Codemirror from "react-codemirror";
import _ from "lodash";

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
    const instance = codeMirrorComponent.getCodeMirrorInstance();
    this.editor = instance;
    instance.commands.save = (_cm) => {
      this.props.onSave();
    };
    instance.commands.run = (_cm) => {
      this.props.onRun();
    };
    instance.commands.autoComplete = (cm) => {
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

    const dataSource = this.props.settings.selectedDataSource();
    const selectedTables = Array.from(this.props.settings.tables);
    const list = _.chain([]).
      union(
        this.props.completions,
        _.map(selectedTables, (table) => ({text: `${dataSource.id}/${table}`})),
        _.map(this.editor.hint.anyword(cm, {word: /[a-zA-Z_](\w)*/}).list, (word) => ({text: word}))
      ).
      filter((candidate) => candidate.text.match(fuzzyMatcher)).
      uniqBy((el) => el.displayText || el.text).
      sortBy((el) => sortOrder(el.text)).
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
      autofocus: true,
      indentUnit: 2,
      indentWithTabs: false,
      lineNumbers: true,
      lineWrapping: true,
      matchBrackets: true,
      mode: "text/x-sql",
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,

      extraKeys: {
        "Ctrl-Enter": "run",
        "Cmd-Enter": "run",
        "Ctrl-Space": "autoComplete",
      },
    };

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

  settings: React.PropTypes.shape({
    selectedDataSource: React.PropTypes.func.isRequired,
    tables: React.PropTypes.object,
  }),
  completions: React.PropTypes.arrayOf(React.PropTypes.shape({
    displayText: React.PropTypes.string,
    text: React.PropTypes.string,
  })),
  statement: React.PropTypes.string,
};
