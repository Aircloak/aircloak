// @flow

import React from "react";

export type Authentication = {CSRFToken: string};
export type AuthContextType = {
  authentication: Authentication
};

export const AuthContext = React.createContext<AuthContextType>({});

type Props = {
  children: ?React$Node,
  authentication: Authentication,
}

export class AuthenticationProvider extends React.Component<Props> {
  render() {
    const authContext = {
      authentication: this.props.authentication,
    };
    return (
      <AuthContext.Provider value={authContext}>
        {this.props.children}
      </AuthContext.Provider>
    );
  }
}
