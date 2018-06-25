// @flow

import React from "react";

export default class PasswordField extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return <div>
        <input type="password" {...this.props}></input>
      </div>;
  }
}
