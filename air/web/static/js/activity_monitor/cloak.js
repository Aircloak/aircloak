// @flow

import React from "react";

export type Cloak = {
  id: string,
  name: string,
  total_memory: number,
  free_memory: {
    current: number,
    last_5_seconds: number,
    last_1_minute: number,
    last_5_minutes: number,
    last_15_minutes: number,
    last_1_hour: number,
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

const renderFreeMemory = (freeMemory) => {
  if (freeMemory === null) {
    return (
      <td>
        <span className={freeMemoryDisplayClasses(0)}>
          Uknown
        </span>
      </td>
    );
  } else {
    return (
      <td>
        <span className={freeMemoryDisplayClasses(freeMemory.last_5_seconds)}>
          5 sec - {toGBstring(freeMemory.last_5_seconds)}
        </span>
        {' '}
        <span className={freeMemoryDisplayClasses(freeMemory.last_5_minutes)}>
          5 min - {toGBstring(freeMemory.last_5_minutes)}
        </span>
        {' '}
        <span className={freeMemoryDisplayClasses(freeMemory.last_15_minutes)}>
          15 min - {toGBstring(freeMemory.last_15_minutes)}
        </span>
        {' '}
        <span className={freeMemoryDisplayClasses(freeMemory.last_1_hour)}>
          1 hour - {toGBstring(freeMemory.last_1_hour)}
        </span>
      </td>
    );
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
    {renderFreeMemory(props.free_memory)}
  </tr>;
