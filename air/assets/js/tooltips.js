// @flow

import _ from "lodash";
import $ from "jquery";
import bootstrap from "bootstrap";

export const activateTooltips = () => _.defer(() => $("[data-toggle='tooltip']").tooltip());
