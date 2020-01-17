// @flow

import React from "react";

type Props = {
  newTableURL: string,
  newViewURL: string,
  supportsCreateTable: boolean,
};

export class NewSelectableToolbarView extends React.Component<Props> {
  conditionallyRenderNewTableButton() {
    const {supportsCreateTable, newTableURL} = this.props;
    if (supportsCreateTable) {
      return <a href={newTableURL} className="btn btn-default">New table</a>;
    } else {
      return null;
    }
  }

  render() {
    const {newViewURL} = this.props;
    return (
      <div className="btn-group btn-group-sm" role="group" aria-label="Create view">
        <a href={newViewURL} className="btn btn-default">New view</a>
        {this.conditionallyRenderNewTableButton()}
      </div>
    );
  }
}
