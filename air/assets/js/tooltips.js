// @flow

import _ from "lodash";
import $ from "jquery";

export const activateTooltips = () => _.defer(() => $("[data-toggle='tooltip']").tooltip());
