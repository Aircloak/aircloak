// @flow

import React from "react";

export type Cloak = {
  id: string,
  name: string,
  free_memory: number,
  total_memory: number,
};

const toMb = (memory_bytes: number) => {
  if (memory_bytes === null) {
    return "Unknown";
  } else {
    return `${Math.floor(memory_bytes / (1024 * 1024))} MB`;
  }

}

export const CloakView = (props: Cloak) =>
  <tr>
    <td>{props.name}</td>
    <td>{toMb(props.total_memory)}</td>
    <td>{toMb(props.free_memory)}</td>
  </tr>;
