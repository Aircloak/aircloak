// @flow

const finalStates = ["completed", "cancelled", "error"];

export const format = (state: string) => {
  switch (state) {
    case "awaiting_data": return "waiting for database";
    case "ingesting_data": return "ingesting data";
    case "post_processing": return "post-processing";
    default: return state;
  }
};

export const isFinished = (state: string) => finalStates.includes(state);

export const pendingStates = [
  "started", "parsing", "compiling", "awaiting_data", "ingesting_data", "processing", "post_processing",
];

export const later = (state1: string, state2: string) =>
  pendingStates.findIndex((s) => s === state1) > pendingStates.findIndex((s) => s === state2);
