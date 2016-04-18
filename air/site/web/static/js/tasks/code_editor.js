import React from "react"
import Codemirror from "react-codemirror"
import CodeMirror from "codemirror"
var _ = require("lodash");

require("codemirror/mode/lua/lua");
require("codemirror/addon/hint/show-hint");
require("codemirror/addon/hint/anyword-hint");

export class CodeEditor extends React.Component {
  constructor(props) {
    super(props);
    this.setupComponent = this.setupComponent.bind(this);
    this.completionList = this.completionList.bind(this);
  }
  completionList(cm) {
    let regex = /(\w|\.)/;
    let cur = cm.getCursor();
    let curLine = cm.getLine(cur.line);
    let start = cur.ch;
    let end = start;

    // find start and end of the word
    while (end < curLine.length && regex.test(curLine.charAt(end))) {
      end++;
    }
    while (start > 0 && regex.test(curLine.charAt(start - 1))) {
      start--;
    }

    let curWord = curLine.slice(start, end);
    let fuzzyMatcher = new RegExp(curWord.replace(/(.)/g, "$1.*"), "i");

    let sortOrder = (text) => {
      // Place items that start with the given word at the top of the completion list.
      if (text.startsWith(curWord)) {
        return "0" + text;
      } else {
        return "1" + text;
      }
    }

    let list = _.chain([]).
        union(
              _.map(CodeMirror.hint.anyword(cm, {word: /[a-zA-Z_](\w)*/}).list,
                (word) => {return {text: word}})
            ).
        filter((candidate) => {return candidate.text.match(fuzzyMatcher)}).
        uniq((el) => {return el.text}).
        sortBy((el) => {return sortOrder(el.text)}).
        value();

    return {
      list: list,
      from: CodeMirror.Pos(cur.line, start),
      to: CodeMirror.Pos(cur.line, end)
    };
  }
  setupComponent(codeMirrorComponent) {
    let instance = codeMirrorComponent.getCodeMirrorInstance();
    instance.commands.save = (_cm) => {
      this.props.onSave();
    }
    instance.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList})
    }
  }
  render() {
    var options = {
      autofocus: true,
      indentUnit: 2,
      indentWithTabs: false,
      lineNumbers: true,
      lineWrapping: true,
      matchBrackets: true,
      mode: "lua",
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,

      extraKeys: {
        "Ctrl-S": "save",
        "Ctrl-Space": "autoComplete"
      }
    };
    return (
          <Codemirror
              ref={this.setupComponent}
              value={this.props.query}
              onChange={this.props.onChange}
              options={options} />
        );
  }
};
