// @flow

import React from "react";
import PropTypes from "prop-types";

export class AuthenticationProvider extends React.Component {
  getChildContext() {
    return {authentication: this.props.authentication};
  }

  render() {
    return <div>{this.props.children}</div>;
  }
}

AuthenticationProvider.propTypes = {
  children: PropTypes.element,
  authentication: PropTypes.object.isRequired,
};

AuthenticationProvider.childContextTypes = {
  authentication: PropTypes.object.isRequired,
};
