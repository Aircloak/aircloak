// @flow

const finalStates = ["completed", "cancelled", "error"];

export const format = (state: string) => state.replace("_", " ");

export const isFinished = (state: string) => finalStates.includes(state);

export const isPending = (state: string) => !isFinished(state);

export const pendingStates = [
  "started", "parsing", "compiling", "awaiting_data", "ingesting_data", "processing", "post_processing"
];

export const later = (state1: string, state2: string) =>
  pendingStates.findIndex((s) => s === state1) > pendingStates.findIndex((s) => s === state2);
