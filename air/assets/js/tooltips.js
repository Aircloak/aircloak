// @flow

import _ from "lodash";
import $ from "jquery";
import "./vendor/bootstrap";

export default () => _.defer(() => $("[data-toggle='tooltip']").tooltip());
