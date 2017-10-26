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

AuthenticationProvider.propTypes = {
  children: React.PropTypes.element,
  authentication: React.PropTypes.object.isRequired,
};

AuthenticationProvider.childContextTypes = {
  authentication: React.PropTypes.object.isRequired,
};
