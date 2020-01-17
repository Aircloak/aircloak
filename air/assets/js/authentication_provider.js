// @flow

import React from "react";
import PropTypes from "prop-types";

type Props = {
  children: ?React$Node,
  authentication: {
    CSRFToken: string,
  },
}

export class AuthenticationProvider extends React.Component<Props> {
  getChildContext() {
    return {authentication: this.props.authentication};
  }

  render() {
    return <React.Fragment>{this.props.children}</React.Fragment>;
  }
}
