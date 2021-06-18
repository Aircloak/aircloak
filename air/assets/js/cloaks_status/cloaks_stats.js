// @flow

import type { Element } from "React";
import React from "react";

import { CloakStatsView } from "./cloak_stats";
import type { CloakStat } from "./cloak_stats";

const renderCloaks = (cloakStats: CloakStat[]) => {
  if (cloakStats.length > 0) {
    return cloakStats.map((cloakStat) => (
      <CloakStatsView key={cloakStat.id} cloakStat={cloakStat} />
    ));
  } else {
    return (
      <tr>
        <td colSpan="3">Currently no cloaks are online.</td>
      </tr>
    );
  }
};

export default ({
  cloakStats,
}: {
  cloakStats: CloakStat[],
}): Element<"div"> => {
  return (
    <div>
      <h3>Cloaks</h3>
      <table className="table table-responsive-lg">
        <thead>
          <tr>
            <th>Name</th>
            <th>Current memory</th>
            <th>% memory usage over time</th>
            <th># queries over time</th>
          </tr>
        </thead>
        <tbody>{renderCloaks(cloakStats)}</tbody>
      </table>
    </div>
  );
};
