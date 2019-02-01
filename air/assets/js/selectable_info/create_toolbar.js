// @flow

import React from "react";

type Props = {
  newViewURL: string,
};

export class CreateToolbarView extends React.Component {
  props: Props;

  render() {
    return (
      <div>
        <div className="btn-group btn-group-sm" role="group" aria-label="Create view">
          <a href={this.props.newViewURL} className="btn btn-default">Create view</a>
        </div>
      </div>
    );
  }
}
