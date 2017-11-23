// @flow

import React from "react";
import pagedown from "pagedown";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

export const Info = (props: {info: string[]}) => {
  if (props.info && props.info.length > 0) {
    return (
      <div>
        <h4>Info</h4>
        {props.info.map((infoMessage, i) =>
          <p key={i} dangerouslySetInnerHTML={mdToHtml(infoMessage)} />)
        }
      </div>
    );
  } else {
    return null;
  }
};
