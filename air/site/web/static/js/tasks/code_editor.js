import React from "react"
import Codemirror from "react-codemirror"

require("codemirror/mode/lua/lua");

export class CodeEditor extends React.Component {
  render() {
    var options = {
      lineNumbers: true,
      mode: "lua"
    };
    return (
          <Codemirror
              value={this.props.query}
              onChange={this.props.onChange}
              options={options} />
        );
  }
};
