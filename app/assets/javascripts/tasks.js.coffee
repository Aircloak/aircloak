//= require angular
//= require codemirror
//= require codemirror/modes/lua
//= require codemirror/keymaps/vim

@editor = CodeMirror.fromTextArea(document.getElementById("task_code"), { lineNumbers: true, mode: "lua", vimMode: true, matchBrackets: true, showCursorWhenSelecting: true })
@app = angular.module("taskEditor", [])
@TaskEditorCtrl = ->
