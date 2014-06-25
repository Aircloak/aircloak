//= require codemirror
//= require codemirror/modes/lua
//= require codemirror/keymaps/vim

CodeMirror.fromTextArea(document.getElementById("task_prefetch"), {
  lineNumbers: true, mode: "json", vimMode: true, matchBrackets: true, showCursorWhenSelecting: true
})
CodeMirror.fromTextArea(document.getElementById("task_code"), {
  lineNumbers: true, mode: "lua", vimMode: true, matchBrackets: true, showCursorWhenSelecting: true
})
