// @flow

import React from "react";

import {CloakView} from "./cloak";
import type {Cloak} from "./cloak";

const renderCloaks = (cloaks: Cloak[]) => {
  if (cloaks.length > 0) {
    return cloaks.map((cloak) =>
      <CloakView key={cloak.id} {...cloak} />
    );
  } else {
    return (
      <tr>
        <td colSpan="3">
          Currently no cloaks are online.
        </td>
      </tr>
    );
  }
};

export const CloaksView = (props: {cloaks: Cloak[]}) =>
  <div>
    <h3>Cloaks</h3>
    <table className="table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Total memory</th>
          <th>Lowest amount of available memory</th>
        </tr>
      </thead>
      <tbody>
        {renderCloaks(props.cloaks)}
      </tbody>
    </table>
  </div>;
