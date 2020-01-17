// @flow
//
// webpack automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "webpack.config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "../css/app.css";
import "phoenix_html";
import "eonasdan-bootstrap-datetimepicker";
import React from "react";
import ReactDOM from "react-dom";
import codeMirror from "codemirror";
import $ from "jquery";

import QueriesView from "./queries/root";
import SingleQueryView from "./queries/single_query_root";
import ImmutableSingleQueryView from "./queries/immutable_single_query";
import SelectableInfoView from "./selectable_info/root";
import EditorView from "./view/editor";
import ActivityMonitorView from "./activity_monitor/root";
import {AuthenticationProvider} from "./authentication_provider";
import {FrontendSocket} from "./frontend_socket";
import {NumberFormatExampleView} from "./number_format";
import AuditLogView from "./audit_log/root";
import PasswordField from "./password_field";


require("core-js");
require("codemirror/mode/markdown/markdown");

const App = {
  queryPage: (props, elem) => App.render("queries", props, elem),
  queryShowPage: (props, elem) => App.render("query_show", props, elem),
  immutableQueryShowPage: (props, elem) => App.render("immutable_query_show", props, elem),
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
    elementEditor.on("change", (editor) => {
      targetElement.value = editor.getValue(); // eslint-disable-line no-param-reassign
    });
  },

  render: (page, props, elem) => {
    const authentication = {CSRFToken: props.CSRFToken};

    ReactDOM.render(
      <AuthenticationProvider authentication={authentication}>
        {App.renderPage(page, props)}
      </AuthenticationProvider>,
      elem,
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
    const {
      authentication,
      dataSourceDescription,
      dataSourceName,
      dataSourceStatus,
      debugModeEnabled,
      lastQuery,
      numberFormat,
      pendingQueries,
      result,
      selectables,
      sessionId,
      socketToken,
      userId,
      selectablesEditUrl,
      newTableUrl,
      newViewURL,
      supportsCreateTable,
      selectableToExclude,
      statement,
      queries,
      auditLogs,
      cloak_stats,
      value,
      score,
    } = props;
    switch (page) {
      case "queries":
        return (
          <QueriesView
            userId={userId}
            sessionId={sessionId}
            socketToken={socketToken}
            dataSourceName={dataSourceName}
            dataSourceDescription={dataSourceDescription}
            dataSourceStatus={dataSourceStatus}
            selectables={selectables}
            lastQuery={lastQuery}
            pendingQueries={pendingQueries}
            frontendSocket={App.buildSocket(props)}
            numberFormat={numberFormat}
            debugModeEnabled={debugModeEnabled}
          />
        );
      case "query_show": 
        return (
          <SingleQueryView
            result={result}
            numberFormat={numberFormat}
            debugModeEnabled={debugModeEnabled}
            frontendSocket={App.buildSocket(props)}
          />
        );
      case "immutable_query_show": 
        return (
          <ImmutableSingleQueryView 
            result={result}
            numberFormat={numberFormat}
            debugModeEnabled={debugModeEnabled}
            authentication={authentication}
          />
        );
      case "selectable_info":
        return (
          <SelectableInfoView
            selectables={selectables}
            selectablesEditUrl={selectablesEditUrl}
            newTableURL={newTableUrl}
            newViewURL={newViewURL}
            userId={userId}
            dataSourceName={dataSourceName}
            dataSourceDescription={dataSourceDescription}
            dataSourceStatus={dataSourceStatus}
            supportsCreateTable={supportsCreateTable}
            selectableToExclude={selectableToExclude}
            frontendSocket={App.buildSocket(props)}
          />
        );
      case "view_editor":
        return (
          <EditorView
            statement={statement}
            selectables={selectables}
          />
        );
      case "activity_monitor": 
        return (
          <ActivityMonitorView 
            userId={userId}
            socketToken={socketToken}
            frontendSocket={App.buildSocket(props)}
            queries={queries}
            cloak_stats={cloak_stats}
          />
        );
      case "number_format_example": 
        return (
          <NumberFormatExampleView
            numberFormat={numberFormat}
          />
        );
      case "audit_log":
        return (
          <AuditLogView
            auditLogs={auditLogs}
          />
        );
      case "password_field":
        return (
          <PasswordField
            value={value}
            score={score}
          />
        );
      default: throw new Error("Unknown page");
    }
  },

  buildSocket: (props) => new FrontendSocket(props.browserSocketTransport, props.socketToken),
};

if (window.pageConfig !== undefined) {
  window.pageConfig.onLoad.call(this, App);
}
