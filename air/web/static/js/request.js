// @flow

import $ from "jquery";

export type Authentication = {CSRFToken: string};

const headers = ({CSRFToken}) => ({
  "X-CSRF-TOKEN": CSRFToken,
  "Content-Type": "application/json",
});

export const cancel = (queryId: string, authentication: Authentication) =>
  $.ajax("/queries/cancel", {
    method: "POST",
    headers: headers(authentication),
    data: JSON.stringify({id: queryId}),
  });

export const startQuery = (queryData, authentication, callbacks) => {
  $.ajax("/queries", {
    method: "POST",
    headers: headers(authentication),
    data: queryData,
    success: callbacks.success,
    error: callbacks.error,
  });
};

export const loadHistory = (dataSourceId, before, authentication, callbacks) => {
  $.ajax(`/queries/load_history/${dataSourceId}?before=${before}`, {
    method: "GET",
    headers: headers(authentication),
    success: callbacks.success,
    error: callbacks.error,
  });
};
