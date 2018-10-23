// @flow

import React from "react";

import {CloakView} from "./cloak_stats";
import type {CloakStat} from "./cloak_stats";

const renderCloaks = (cloakStats: CloakStat[]) => {
  if (cloakStats.length > 0) {
    return cloakStats.map((cloakStat) =>
      <CloakView key={cloakStat.id} {...cloakStat} />
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

export const CloaksView = (props: {cloakStats: CloakStat[]}) =>
  <div>
    <h3>Cloaks</h3>
    <table className="table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Current memory</th>
          <th>% memory usage over time</th>
          <th># queries over time</th>
        </tr>
      </thead>
      <tbody>
        {renderCloaks(props.cloakStats)}
      </tbody>
    </table>
  </div>;
