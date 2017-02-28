// @flow

import React from "react";

export type Cloak = {
  id: string,
  name: string,
  total_memory: number,
  free_memory: {
    current: number,
    last_five_seconds: number,
    last_minute: number,
    last_five_minutes: number,
    last_fifteen_minutes: number,
    last_hour: number,
  },
};

const toGB = (memoryInBytes: number) => {
  if (memoryInBytes === null || memoryInBytes === undefined) {
    return 0;
  } else {
    // Not exactly accurate, but a good enough approximation
    return Math.floor(memoryInBytes / (1024 * 1024 * 102)) / 10;
  }
};

const toGBstring = (memoryInBytes: number) => {
  if (memoryInBytes === null) {
    return "Unknown";
  } else {
    // Not exactly accurate, but a good enough approximation
    return `${toGB(memoryInBytes)} GB`;
  }
};

const totalMemoryDisplayClasses = (memoryInBytes) => {
  const memoryInGB = toGB(memoryInBytes);
  if (memoryInGB <= 2) {
    return "label label-danger";
  } else if (memoryInGB <= 4) {
    return "label label-warning";
  } else {
    return "label label-success";
  }
};

const freeMemoryDisplayClasses = (memoryInBytes) => {
  const memoryInGB = toGB(memoryInBytes);
  if (memoryInGB <= 0.5) {
    return "label label-danger";
  } else if (memoryInGB <= 1.5) {
    return "label label-warning";
  } else {
    return "label label-success";
  }
};

export const CloakView = (props: Cloak) =>
  <tr>
    <td>{props.name}</td>
    <td>
      <span className={totalMemoryDisplayClasses(props.total_memory)}>
        {toGBstring(props.total_memory)}
      </span>
    </td>
    <td>
      <span className={freeMemoryDisplayClasses(props.free_memory.last_five_seconds)}>
        5 sec - {toGBstring(props.free_memory.last_five_seconds)}
      </span>
      {' '}
      <span className={freeMemoryDisplayClasses(props.free_memory.last_five_minutes)}>
        5 min - {toGBstring(props.free_memory.last_five_minutes)}
      </span>
      {' '}
      <span className={freeMemoryDisplayClasses(props.free_memory.last_fifteen_minutes)}>
        15 min - {toGBstring(props.free_memory.last_fifteen_minutes)}
      </span>
      {' '}
      <span className={freeMemoryDisplayClasses(props.free_memory.last_hour)}>
        1 hour - {toGBstring(props.free_memory.last_hour)}
      </span>
    </td>
  </tr>;
