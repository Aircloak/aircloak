// @flow

import React from "react";

type Props = {
  newTableURL: string,
  newViewURL: string,
  supportsCreateTable: boolean,
};

export class NewSelectableToolbarView extends React.Component<Props> {
  props: Props;

  conditionallyRenderNewTableButton() {
    if (this.props.supportsCreateTable) {
      return <a href={this.props.newTableURL} className="btn btn-default">New table</a>;
    } else {
      return null;
    }
  }

  render() {
    return (
      <div className="btn-group btn-group-sm" role="group" aria-label="Create view">
        <a href={this.props.newViewURL} className="btn btn-default">New view</a>
        {this.conditionallyRenderNewTableButton()}
      </div>
    );
  }
}
