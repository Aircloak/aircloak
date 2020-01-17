// @flow

import React from "react";
import _ from "lodash";
import {Sparklines, SparklinesLine} from "react-sparklines";

export type CloakStat = {
  id: string,
  name: string,
  stats: {
    memory: {
      total: number,
      currently_in_use: number,
      in_use_percent: number,
      readings: [number]
    },
    queries: [number]
  }
};

type Props = {
  cloakStat: CloakStat
}

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
    return "label label-danger";
  } else if (memoryUtilisationPercentage >= 60) {
    return "label label-warning";
  } else {
    return "label label-success";
  }
};

const renderCurrentMemoryUtilisation = (memory) =>
  <td>
    <span className={memoryUtilisationClasses(memory.in_use_percent)}>
      {toGBstring(memory.currently_in_use)} / {toGBstring(memory.total)}
    </span>
  </td>;

const renderMemoryUtilisationGraph = (readings) =>
  <td>
    <Sparklines data={readings} svgHeight={25} svgWidth={190} min={0} max={100}>
      <SparklinesLine />
    </Sparklines>
  </td>;

const renderQueriesGraph = (queryStats) => {
  const maxQueriesStat = _.max([1, ...queryStats]);
  return (
    <td>
      <Sparklines data={queryStats} svgHeight={25} svgWidth={190} min={0} max={maxQueriesStat}>
        <SparklinesLine />
      </Sparklines>
    </td>
  );
};

export class CloakStatsView extends React.PureComponent<Props> {
  render() {
    return (
      <tr>
        <td>{this.props.cloakStat.name}</td>
        {renderCurrentMemoryUtilisation(this.props.cloakStat.stats.memory)}
        {renderMemoryUtilisationGraph(this.props.cloakStat.stats.memory.readings)}
        {renderQueriesGraph(this.props.cloakStat.stats.queries)}
      </tr>
    );
  }
}
