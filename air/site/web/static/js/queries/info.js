import React from "react";

export const Info = (props) => {
  if (props.info) {
    return (
      <div>
        <h4>Info</h4>
        <p>{props.info}</p>
      </div>
    );
  } else {
    return null;
  }
};

Info.propTypes = {
  info: React.PropTypes.string,
};
