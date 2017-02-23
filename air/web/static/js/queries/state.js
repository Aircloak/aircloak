// @flow

const finalStates = ["completed", "cancelled", "error"];

export const format = (state) => state.replace("_", " ");

export const isFinished = (state) => finalStates.includes(state);

export const isPending = (state) => !isFinished(state);
