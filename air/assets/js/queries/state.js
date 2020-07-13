// @flow

const finalStates = ["completed", "cancelled", "error"];

export const format = (state: string) => {
  switch (state) {
    case "awaiting_data":
      return "waiting for database";
    case "ingesting_data":
      return "ingesting data";
    case "post_processing":
      return "post-processing";
    default:
      return state;
  }
};

export const isFinished = (state: string) => finalStates.includes(state);

export const pendingStates: Array<string> = [
  "created",
  "started",
  "parsing",
  "compiling",
  "awaiting_data",
  "ingesting_data",
  "processing",
  "post_processing",
];

const completedStates: Array<string> = ["cancelled", "completed"];

export const allStates: Array<string> = pendingStates.concat(completedStates);

export const later = (state1: string, state2: string) =>
  allStates.findIndex((s) => s === state1) >
  allStates.findIndex((s) => s === state2);
