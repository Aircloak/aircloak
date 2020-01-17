// @flow

import React from "react";
import pagedown from "pagedown";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

export const Info = (props: {info: string[]}) => {
  const {info} = props;
  if (info && info.length > 0) {
    return (
      <div>
        <h4>Info</h4>
        {info.map((infoMessage, i) => <p key={i} dangerouslySetInnerHTML={mdToHtml(infoMessage)} />)}
      </div>
    );
  } else {
    return null;
  }
};
