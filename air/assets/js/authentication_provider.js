// @flow

import React from "react";
import PropTypes from "prop-types";

export class AuthenticationProvider extends React.Component {
  getChildContext() {
    return {authentication: this.props.authentication};
  }

  render() {
    return <React.Fragment>{this.props.children}</React.Fragment>;
  }
}

AuthenticationProvider.propTypes = {
  children: PropTypes.element,
  authentication: PropTypes.object.isRequired,
};

AuthenticationProvider.childContextTypes = {
  authentication: PropTypes.object.isRequired,
};
