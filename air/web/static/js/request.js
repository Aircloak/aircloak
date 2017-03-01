// @flow

export const cancel = (queryId, csrfToken) =>
  $.ajax("/queries/cancel", {
    method: "POST",
    headers: {
      "X-CSRF-TOKEN": csrfToken,
      "Content-Type": "application/json",
    },
    data: JSON.stringify({id: queryId}),
  });
