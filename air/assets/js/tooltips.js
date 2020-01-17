// @flow

import _ from "lodash";
import $ from "jquery";
import "./vendor/bootstrap";

export const activateTooltips = () => _.defer(() => $("[data-toggle='tooltip']").tooltip());
