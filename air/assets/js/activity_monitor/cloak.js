// @flow

import React from "react";

export type Cloak = {
  id: string,
  name: string,
  total_memory: number,
  available_memory: {
    current: number,
    last_5_seconds: number,
    last_1_minute: number,
    last_5_minutes: number,
    last_15_minutes: number,
    last_1_hour: number,
  },
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

const totalMemoryDisplayClasses = (memoryInKBytes) => {
  const memoryInGB = toGB(memoryInKBytes);
  if (memoryInGB <= 2) {
    return "label label-danger";
  } else if (memoryInGB <= 4) {
    return "label label-warning";
  } else {
    return "label label-success";
  }
};

const availableMemoryDisplayClasses = (memoryInKBytes) => {
  const memoryInGB = toGB(memoryInKBytes);
  if (memoryInGB <= 0.5) {
    return "label label-danger";
  } else if (memoryInGB <= 1.5) {
    return "label label-warning";
  } else {
    return "label label-success";
  }
};

const renderAvailableMemory = (availableMemory) => {
  if (availableMemory === null) {
    return (
      <td>
        <span className={availableMemoryDisplayClasses(0)}>
          Uknown
        </span>
      </td>
    );
  } else {
    return (
      <td>
        <span className={availableMemoryDisplayClasses(availableMemory.last_5_seconds)}>
          5 sec - {toGBstring(availableMemory.last_5_seconds)}
        </span>
        {' '}
        <span className={availableMemoryDisplayClasses(availableMemory.last_5_minutes)}>
          5 min - {toGBstring(availableMemory.last_5_minutes)}
        </span>
        {' '}
        <span className={availableMemoryDisplayClasses(availableMemory.last_15_minutes)}>
          15 min - {toGBstring(availableMemory.last_15_minutes)}
        </span>
        {' '}
        <span className={availableMemoryDisplayClasses(availableMemory.last_1_hour)}>
          1 hour - {toGBstring(availableMemory.last_1_hour)}
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
    {renderAvailableMemory(props.available_memory)}
  </tr>;
