// @flow

const finalStates = ["completed", "cancelled", "error"];

export const format = (state: string) => state.replace("_", " ");

export const isFinished = (state: string) => finalStates.includes(state);

export const isPending = (state: string) => !isFinished(state);
