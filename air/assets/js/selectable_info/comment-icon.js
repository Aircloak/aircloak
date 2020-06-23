import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

const CommentIcon = ({ comment }) => (
  <OverlayTrigger placement="left" overlay={<Tooltip>{comment}</Tooltip>}>
    <i className="fas fa-sticky-note text-warning"></i>
  </OverlayTrigger>
);

export default CommentIcon;
