// @flow

import React from "react";

export class AuthenticationProvider extends React.Component {
  getChildContext() {
    return {authentication: this.props.authentication};
  }

  render() {
    return <div>{this.props.children}</div>;
  }
}

AuthenticationProvider.childContextTypes = {
  authentication: React.PropTypes.shape({
    CSRFToken: React.PropTypes.string
  })
}
