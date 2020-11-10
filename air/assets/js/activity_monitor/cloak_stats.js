// @flow

import type { Element } from "React";
import React from "react";
import { Sparklines, SparklinesLine } from "react-sparklines";

export type CloakStat = {
  id: string,
  name: string,
  stats: {
    memory: {
      total: number,
      currently_in_use: number,
      in_use_percent: number,
      readings: [number],
    },
    queries: [number],
  },
};

type Props = {
  cloakStat: CloakStat,
};

const toGB = (memoryInKBytes: number) => {
  if (memoryInKBytes === null || memoryInKBytes === undefined) {
    return 0;
  } else {
    // Not exactly accurate, but a good enough approximation
    return Math.floor(memoryInKBytes / (1024 * 102)) / 10;
  }
};

const toGBstring = (memoryInKBytes: number) => {
  if (memoryInKBytes === null) {
    return "Unknown";
  } else {
    // Not exactly accurate, but a good enough approximation
    return `${toGB(memoryInKBytes)} GB`;
  }
};

const memoryUtilisationClasses = (memoryUtilisationPercentage) => {
  if (memoryUtilisationPercentage >= 95) {
    return "badge badge-danger";
  } else if (memoryUtilisationPercentage >= 60) {
    return "badge badge-warning";
  } else {
    return "badge badge-success";
  }
};

const renderCurrentMemoryUtilisation = (memory) => (
  <td>
    <span className={memoryUtilisationClasses(memory.in_use_percent)}>
      {toGBstring(memory.currently_in_use)}
      {" / "}
      {toGBstring(memory.total)}
    </span>
  </td>
);

const renderMemoryUtilisationGraph = (readings) => (
  <td>
    <Sparklines data={readings} svgHeight={25} svgWidth={190} min={0} max={100}>
      <SparklinesLine />
    </Sparklines>
  </td>
);

const renderQueriesGraph = (queryStats) => {
  const maxQueriesStat = Math.max(1, ...queryStats);
  return (
    <td>
      <Sparklines
        data={queryStats}
        svgHeight={25}
        svgWidth={190}
        min={0}
        max={maxQueriesStat}
      >
        <SparklinesLine />
      </Sparklines>
    </td>
  );
};

export class CloakStatsView extends React.PureComponent<Props> {
  render(): Element<"tr"> {
    const { cloakStat } = this.props;
    return (
      <tr>
        <td>{cloakStat.name}</td>
        {renderCurrentMemoryUtilisation(cloakStat.stats.memory)}
        {renderMemoryUtilisationGraph(cloakStat.stats.memory.readings)}
        {renderQueriesGraph(cloakStat.stats.queries)}
      </tr>
    );
  }
}
