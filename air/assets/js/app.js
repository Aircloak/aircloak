// @flow
import "../css/app.css";
import "phoenix_html";
import React from "react";
import ReactDOM from "react-dom";
import codeMirror from "codemirror";
import CodeViewer from "./code_viewer";
import QueriesView from "./queries/root";
import SingleQueryView from "./queries/single_query_root";
import ImmutableSingleQueryView from "./queries/immutable_single_query";
import SelectableInfoView from "./selectable_info/root";
import ViewEditor from "./view/editor";
import ActivityMonitorView from "./activity_monitor/root";
import AuthenticationProvider from "./authentication_provider";
import FrontendSocket from "./frontend_socket";
import { NumberFormatExampleView } from "./number_format";
import PasswordField from "./password_field";
import activateDatetimePickers from "./datetimepicker";
import "codemirror/mode/markdown/markdown";
import activateTooltips from "./tooltips";
import copyToClipboard from "./copy_to_clipboard";
import liveView from "./live_view";

activateTooltips();
activateDatetimePickers();
window.copyToClipboard = copyToClipboard;

const App = {
  queryPage: (props, elem) => App.render("queries", props, elem),
  queryShowPage: (props, elem) => App.render("query_show", props, elem),
  immutableQueryShowPage: (props, elem) =>
    App.render("immutable_query_show", props, elem),
  codeViewer: (props, elem) => App.render("code_viewer", props, elem),
  selectableInfo: (props, elem) => App.render("selectable_info", props, elem),
  viewEditor: (props, elem) => App.render("view_editor", props, elem),
  activityMonitor: (props, elem) => App.render("activity_monitor", props, elem),
  numberFormatExample: (props, elem) =>
    ReactDOM.render(
      <NumberFormatExampleView numberFormat={props.numberFormat} />,
      elem
    ),
  passwordField: (props, elem) => App.render("password_field", props, elem),
  liveView,

  attachCodeMirrorToTextArea: (textArea, targetElement) => {
    const elementEditor = codeMirror(
      (elt) => {
        textArea.parentNode.replaceChild(elt, textArea);
      },
      {
        value: textArea.value,
        indentWithTabs: false,
        tabSize: 2,
        mode: "markdown",
        lineNumbers: true,
      }
    );
    elementEditor.on("change", (editor) => {
      targetElement.value = editor.getValue(); // eslint-disable-line no-param-reassign
    });
  },

  render: (page, props, elem) => {
    const authentication = { CSRFToken: props.CSRFToken };

    ReactDOM.render(
      <AuthenticationProvider authentication={authentication}>
        {App.renderPage(page, props)}
      </AuthenticationProvider>,
      elem
    );
  },

  activateDatetimePickers,

  renderPage: (page, props) => {
    const {
      authentication,
      cloakStats,
      dataSourceDescription,
      dataSourceName,
      dataSourceStatus,
      debugModeEnabled,
      initialError,
      lastQuery,
      newTableURL,
      newViewURL,
      numberFormat,
      pendingQueries,
      queries,
      result,
      selectables,
      selectablesEditUrl,
      selectableToExclude,
      sessionId,
      socketToken,
      statement,
      supportsCreateTable,
      userId,
      typeCheckingEnabled,
    } = props;
    switch (page) {
      case "queries":
        return (
          <QueriesView
            userId={userId}
            sessionId={sessionId}
            socketToken={socketToken}
            dataSourceName={dataSourceName}
            dataSourceStatus={dataSourceStatus}
            selectables={selectables}
            lastQuery={lastQuery}
            pendingQueries={pendingQueries}
            frontendSocket={App.buildSocket(props)}
            numberFormat={numberFormat}
            debugModeEnabled={debugModeEnabled}
            typeCheckingEnabled={typeCheckingEnabled}
          />
        );
      case "query_show":
        return (
          <SingleQueryView
            result={result}
            insertedAt={result.inserted_at}
            numberFormat={numberFormat}
            user={result.user}
            debugModeEnabled={debugModeEnabled}
            frontendSocket={App.buildSocket(props)}
          />
        );
      case "immutable_query_show":
        return (
          <ImmutableSingleQueryView
            result={result}
            user={result.user}
            insertedAt={result.inserted_at}
            numberFormat={numberFormat}
            debugModeEnabled={debugModeEnabled}
            authentication={authentication}
          />
        );
      case "code_viewer":
        return <CodeViewer statement={statement} />;
      case "selectable_info":
        return (
          <SelectableInfoView
            selectables={selectables}
            selectablesEditUrl={selectablesEditUrl}
            newTableURL={newTableURL}
            newViewURL={newViewURL}
            userId={userId}
            dataSourceName={dataSourceName}
            dataSourceDescription={dataSourceDescription}
            dataSourceStatus={dataSourceStatus}
            supportsCreateTable={supportsCreateTable}
            selectableToExclude={selectableToExclude}
            frontendSocket={App.buildSocket(props)}
            numberFormat={numberFormat}
          />
        );
      case "view_editor":
        return <ViewEditor statement={statement} selectables={selectables} />;
      case "activity_monitor":
        return (
          <ActivityMonitorView
            userId={userId}
            socketToken={socketToken}
            frontendSocket={App.buildSocket(props)}
            queries={queries}
            cloakStats={cloakStats}
          />
        );

      case "password_field":
        return <PasswordField initialError={initialError || null} />;
      default:
        throw new Error("Unknown page");
    }
  },

  buildSocket: (props) =>
    new FrontendSocket(props.browserSocketTransport, props.socketToken),
};

if (window.pageConfig !== undefined) {
  window.pageConfig.onLoad.call(this, App);
}
