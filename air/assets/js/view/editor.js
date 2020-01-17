// @flow

import React from "react";
import $ from "jquery";
import _ from "lodash";
import CodeEditor from "../code_editor";
import type {Selectable} from "../selectable_info/selectable";

type Props = {
  statement: string,
  selectables: Selectable[],
}

export default class ViewEditor extends React.Component<Props> {
  static setStatement(statement: string) {
    $("#sql").val(statement);
  }

  tableNames() {
    const {selectables} = this.props;
    return selectables.map<string>((table) => table.id);
  }

  columnNames() {
    const {selectables} = this.props;
    return _.flatMap(selectables, (table) => table.columns.map<string>((column) => column.name));
  }

  static save() {
    $("#viewForm").submit();
  }

  render() {
    const {statement} = this.props;
    return (
      <CodeEditor
        statement={statement}
        tableNames={this.tableNames()}
        columnNames={this.columnNames()}
        onChange={ViewEditor.setStatement}
        onRun={ViewEditor.save}
      />
    );
  }
}
