// @flow
//
// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html";
import React from "react";
import ReactDOM from "react-dom";

import Queries from "./queries/root";
import SingleQuery from "./queries/single_query_root";
import SelectableInfo from "./selectable_info/root";
import Editor from "./view/editor";
import ActivityMonitor from "./activity_monitor/root";
import {AuthenticationProvider} from "./authentication_provider";
import {FrontendSocket} from "./frontend_socket";
import {NumberFormatExample} from "./number_format";

const App = {
  queryPage: (props, elem) => App.render("queries", props, elem),
  queryShowPage: (props, elem) => App.render("query_show", props, elem),
  selectableInfo: (props, elem) => App.render("selectable_info", props, elem),
  viewEditor: (props, elem) => App.render("view_editor", props, elem),
  activityMonitor: (props, elem) => App.render("activity_monitor", props, elem),
  numberFormatExample: (props, elem) => App.render("number_format_example", props, elem),

  render: (page, props, elem) => {
    const authentication = {CSRFToken: props.CSRFToken};

    ReactDOM.render(
      <AuthenticationProvider authentication={authentication}>
        {App.renderPage(page, props)}
      </AuthenticationProvider>,
      elem
    );
  },

  renderPage: (page, props) => {
    switch (page) {
      case "queries": return <Queries frontendSocket={App.buildSocket(props)} {...props} />;
      case "query_show": return <SingleQuery frontendSocket={App.buildSocket(props)} {...props} />;
      case "selectable_info": return <SelectableInfo {...props} />;
      case "view_editor": return <Editor {...props} />;
      case "activity_monitor": return <ActivityMonitor frontendSocket={App.buildSocket(props)} {...props} />;
      case "number_format_example": return <NumberFormatExample {...props} />;
      default: throw new Error("Unknown page");
    }
  },

  buildSocket: (props) => new FrontendSocket(props.guardianToken),
};

if (window.pageConfig !== undefined) {
  window.pageConfig.onLoad.call(this, App);
}
