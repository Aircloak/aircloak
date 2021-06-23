// @flow

import $ from "jquery";

import type { Authentication } from "./authentication_provider";

type Callback = (result: any) => void;
type Callbacks = { success?: Callback, error?: Callback };

const headers = ({ CSRFToken }) => ({
  "X-CSRF-TOKEN": CSRFToken,
  "Content-Type": "application/json",
});

export const cancel = (queryId: string, authentication: Authentication): any =>
  $.ajax(`/queries/${queryId}/cancel`, {
    method: "POST",
    headers: headers(authentication),
  });

export const deleteQueryResult = (
  queryId: string,
  authentication: Authentication
) => {
  $.ajax(`/queries/${queryId}`, {
    method: "DELETE",
    headers: headers(authentication),
  });
};

export const setQueryNote = (
  queryId: string,
  note: string,
  authentication: Authentication
) => {
  $.ajax(`/queries/${queryId}/note`, {
    method: "PATCH",
    headers: headers(authentication),
    data: JSON.stringify({ note }),
  });
};

export const startQuery = (
  queryData: string,
  authentication: Authentication,
  callbacks: Callbacks
) => {
  $.ajax("/queries", {
    method: "POST",
    headers: headers(authentication),
    data: queryData,
    success: callbacks.success,
    error: callbacks.error,
  });
};

export const loadHistory = (
  dataSourceName: string,
  before: string,
  authentication: Authentication,
  callbacks: Callbacks
) => {
  $.ajax(`/queries/load_history/${dataSourceName}?before=${before}`, {
    method: "GET",
    headers: headers(authentication),
    success: callbacks.success,
    error: callbacks.error,
  });
};

export const loadBuckets = (
  bucketsLink: string,
  chunk: number,
  authentication: Authentication,
  callbacks: Callbacks
) => {
  const desiredChunk = chunk >= 0 ? chunk : "all";
  $.ajax(`${bucketsLink}?chunk=${desiredChunk}`, {
    method: "GET",
    headers: headers(authentication),
    success: callbacks.success,
    error: callbacks.error,
  });
};
