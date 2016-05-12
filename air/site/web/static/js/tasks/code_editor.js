import React from 'react';
import Codemirror from 'react-codemirror';
import _ from 'lodash';

require('codemirror/mode/lua/lua');
require('codemirror/addon/hint/show-hint');
require('codemirror/addon/hint/anyword-hint');

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
    }
    instance.commands.run = (_cm) => {
      this.props.onRun();
    }
    instance.commands.autoComplete = (cm) => {
      cm.showHint({hint: this.completionList});
    }
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
    let fuzzyMatcher = new RegExp(curWord.replace(/(.)/g, '$1.*'), 'i');

    let sortOrder = (text) => {
      // Place items that start with the given word at the top of the completion list.
      if (text.startsWith(curWord)) {
        return '0' + text;
      } else {
        return '1' + text;
      }
    }

    let
      dataSource = this.props.settings.selectedDataSource(),
      selectedTables = Array.from(this.props.settings.tables),
      list = _.chain([]).
        union(
          this.props.completions,
          _.map(selectedTables, (table) => {return {text: `load_user_table("${dataSource.id}/${table}")`}}),
          _.map(selectedTables, (table) => {return {text: `user_table("${dataSource.id}/${table}")`}}),
          _.map(this.editor.hint.anyword(cm, {word: /[a-zA-Z_](\w)*/}).list,
            (word) => {return {text: word}})
        ).
        filter((candidate) => {return candidate.text.match(fuzzyMatcher)}).
        uniqBy((el) => {return el.displayText || el.text}).
        sortBy((el) => {return sortOrder(el.text)}).
        value();

    let returnValue = {
      list: list,
      from: this.editor.Pos(cur.line, start),
      to: this.editor.Pos(cur.line, end)
    };
    return returnValue;
  }

  render() {
    var options = {
      autofocus: true,
      indentUnit: 2,
      indentWithTabs: false,
      lineNumbers: true,
      lineWrapping: true,
      matchBrackets: true,
      mode: 'lua',
      showCursorWhenSelecting: true,
      smartIndent: true,
      viewportMargin: Infinity,

      extraKeys: {
        'Ctrl-S': 'save',
        'Ctrl-R': 'run',
        'Ctrl-Space': 'autoComplete'
      }
    };
    return (
      <div className={this.props.sidePaneHidden() ? 'side-panel-hidden' : 'side-panel-shown' }>
        <Codemirror
          ref={this.setupComponent}
          value={this.props.query}
          onChange={this.props.onChange}
          options={options} />
      </div>
    );
  }
};
