// @flow
import $ from "jquery";
import "bootstrap";

export default () =>
  setTimeout(() => $("[data-toggle='tooltip']").tooltip(), 1);
