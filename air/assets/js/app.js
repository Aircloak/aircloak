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
require("core-js");

import "phoenix_html";
import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";

import Queries from "./queries/root";
import SingleQuery from "./queries/single_query_root";
import SelectableInfo from "./selectable_info/root";
import Editor from "./view/editor";
import ActivityMonitor from "./activity_monitor/root";
import {AuthenticationProvider} from "./authentication_provider";
import {FrontendSocket} from "./frontend_socket";
import {NumberFormatExample} from "./number_format";
import AuditLog from "./audit_log/root";
import PasswordField from "./password_field";

import codeMirror from "codemirror";
require("codemirror/mode/markdown/markdown");

const App = {
  queryPage: (props, elem) => App.render("queries", props, elem),
  queryShowPage: (props, elem) => App.render("query_show", props, elem),
  selectableInfo: (props, elem) => App.render("selectable_info", props, elem),
  viewEditor: (props, elem) => App.render("view_editor", props, elem),
  activityMonitor: (props, elem) => App.render("activity_monitor", props, elem),
  numberFormatExample: (props, elem) => App.render("number_format_example", props, elem),
  auditLog: (props, elem) => App.render("audit_log", props, elem),
  passwordField: (props, elem) => App.render("password_field", props, elem),

  attachCodeMirrorToTextArea: (textArea, targetElement) => {
    const elementEditor = codeMirror((elt) => {
      textArea.parentNode.replaceChild(elt, textArea);
    }, {
      value: textArea.value,
      indentWithTabs: false,
      tabSize: 2,
      mode: "markdown",
      lineNumbers: true,
    });
    elementEditor.on("change", editor => {
      targetElement.value = editor.getValue(); // eslint-disable-line no-param-reassign
    });
  },

  render: (page, props, elem) => {
    const authentication = {CSRFToken: props.CSRFToken};

    ReactDOM.render(
      <AuthenticationProvider authentication={authentication}>
        {App.renderPage(page, props)}
      </AuthenticationProvider>,
      elem
    );
  },

  activateDatetimePickers: () => {
    $(".datetimepicker").datetimepicker({
      allowInputToggle: true,
      showTodayButton: true,
      showClose: true,
      format: "YYYY-MM-DD HH:mm:ss",
    });
  },

  renderPage: (page, props) => {
    switch (page) {
      case "queries": return <Queries frontendSocket={App.buildSocket(props)} {...props} />;
      case "query_show": return <SingleQuery frontendSocket={App.buildSocket(props)} {...props} />;
      case "selectable_info": return <SelectableInfo {...props} />;
      case "view_editor": return <Editor {...props} />;
      case "activity_monitor": return <ActivityMonitor frontendSocket={App.buildSocket(props)} {...props} />;
      case "number_format_example": return <NumberFormatExample {...props} />;
      case "audit_log": return <AuditLog {...props} />;
      case "password_field": return <PasswordField {...props} />;
      default: throw new Error("Unknown page");
    }
  },

  buildSocket: (props) => new FrontendSocket(props.guardianToken),
};

if (window.pageConfig !== undefined) {
  window.pageConfig.onLoad.call(this, App);
}
