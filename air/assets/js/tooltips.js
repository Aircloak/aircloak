import _ from "lodash";

export const activateTooltips = () => _.defer(() => $("[data-toggle='tooltip']").tooltip());
