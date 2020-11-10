// @flow

import type { Node } from "React";
import React from "react";
import $ from "jquery";
import CodeEditor from "../code_editor";
import type { Selectable } from "../selectable_info/selectable";

type Props = {
  statement: string,
  selectables: Selectable[],
};

type State = {
  statement: string,
};

export default class ViewEditor extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      statement: props.statement,
    };
  }

  setStatement: (statement: string) => void = (statement: string) => {
    $("#sql").val(statement);
    this.setState({ statement });
  };

  tableNames(): Array<string> {
    const { selectables } = this.props;
    return selectables.map<string>((table) => table.id);
  }

  columnNames(): Array<string> {
    const { selectables } = this.props;
    return selectables.flatMap<string>((table) =>
      table.columns.map<string>((column) => column.name)
    );
  }

  static save() {
    $("#viewForm").submit();
  }

  render(): Node {
    return (
      <CodeEditor
        statement={this.state.statement}
        tableNames={this.tableNames()}
        columnNames={this.columnNames()}
        onChange={this.setStatement}
        onRun={ViewEditor.save}
        annotations={[]}
      />
    );
  }
}
