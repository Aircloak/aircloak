// @flow
import $ from "jquery";
import "bootstrap";

export default (): TimeoutID =>
  setTimeout(() => $("[data-toggle='tooltip']").tooltip(), 1);
