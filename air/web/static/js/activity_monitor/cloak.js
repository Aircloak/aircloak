// @flow

import React from "react";

export type Cloak = {
  id: string,
  name: string,
  free_memory: number,
  total_memory: number,
};

const toGB = (memoryInBytes: number) => {
  if (memoryInBytes === null) {
    return "Unknown";
  } else {
    // Not exactly accurate, but a good enough approximation
    return `${Math.floor(memoryInBytes / (1024 * 1024 * 102)) / 10} GB`;
  }
};

export const CloakView = (props: Cloak) =>
  <tr>
    <td>{props.name}</td>
    <td>{toGB(props.total_memory)}</td>
    <td>{toGB(props.free_memory)}</td>
  </tr>;
