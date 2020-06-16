// @flow

import React from "react";

export type Authentication = { CSRFToken: string };
export type AuthContextType = {
  authentication: Authentication,
};

export const AuthContext = React.createContext<AuthContextType>({});

type Props = {
  children: ?React$Node,
  authentication: Authentication,
};

export default ({ authentication, children }: Props) => {
  const authContextValue = {
    authentication,
  };
  return (
    <AuthContext.Provider value={authContextValue}>
      {children}
    </AuthContext.Provider>
  );
};
