// @flow

import React from "react";

type Props = {
  newViewURL: string,
  supportsCreateTable: boolean,
};

export class CreateToolbarView extends React.Component {
  props: Props;

  conditionallyRenderCreateTable() {
    if (this.props.supportsCreateTable) {
      return <a href="" className="btn btn-default">Create table</a>;
    } else {
      return null;
    }
  }

  render() {
    return (
      <div className="btn-group btn-group-sm" role="group" aria-label="Create view">
        <a href={this.props.newViewURL} className="btn btn-default">Create view</a>
        {this.conditionallyRenderCreateTable()}
      </div>
    );
  }
}
