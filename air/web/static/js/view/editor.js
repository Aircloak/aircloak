// @flow

import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import {CodeEditor} from "../code_editor";

type Props = {
  statement: string,
}

class ViewEditor extends React.Component {
  constructor(props: Props) {
    super(props);
    this.setStatement = this.setStatement.bind(this);
  }
  props: Props;
  setStatement: () => void;

  setStatement(statement: string) {
    $("#sql").val(statement);
  }

  render() {
    return (
      <CodeEditor
        columnNames={[]}
        tableNames={[]}
        statement={this.props.statement}
        onChange={this.setStatement}
        onRun={() => {}}
        onStop={() => {}}
      />
    );
  }
}

export default function renderViewEditor(data: Props, elem: Node) {
  ReactDOM.render(<ViewEditor statement={$("#sql").val()} {...data} />, elem);
}
