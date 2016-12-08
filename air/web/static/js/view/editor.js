// @flow

import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import _ from "lodash";
import {CodeEditor} from "../code_editor";
import type {Selectable} from "../selectable_info/selectable";

type Props = {
  statement: string,
  selectables: Selectable[],
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

  tableNames() {
    return this.props.selectables.map((table) => table.id);
  }

  columnNames() {
    return _.flatMap(this.props.selectables, (table) =>
      table.columns.map((column) => column.name)
    );
  }

  save() {
    $("#viewForm").submit();
  }

  render() {
    return (
      <CodeEditor
        columnNames={[]}
        tableNames={[]}
        statement={this.props.statement}
        tableNames={this.tableNames()}
        columnNames={this.columnNames()}
        onChange={this.setStatement}
        onRun={this.save}
        onStop={() => {}}
      />
    );
  }
}

export default function renderViewEditor(data: Props, elem: Node) {
  ReactDOM.render(<ViewEditor statement={$("#sql").val()} {...data} />, elem);
}
