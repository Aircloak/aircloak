import React from "react";

export class SettingsModel {
  constructor(props) {
    Object.assign(this, props);
  }

  selectedDataSource() {
    return this.dataSources.find((dataSource) => (dataSource.token === this.dataSourceToken));
  }

  selectedDataSourceToken() {
    const selectedDataSource = this.selectedDataSource();
    return selectedDataSource ? selectedDataSource.token : null;
  }

  selectedDataSourceTables() {
    const selectedDataSource = this.selectedDataSource();
    return selectedDataSource ? selectedDataSource.tables : [];
  }

  isTableSelected(table) {
    return this.tables.has(table.id);
  }

  hasAssignedDataSource() {
    return this.dataSourceToken !== null;
  }

  hasAssignedTables() {
    return this.tables.size > 0;
  }

  hasDataSourcesAvailable() {
    return this.dataSources.length > 0;
  }

  // Returns false if and only if the task has been assigned
  // a cloak, and this cloak is not online. That is, it returns
  // true if the task has not yet been assigned a cloak.
  selectedCloakOnline() {
    // some cloaks are connected, but no cloak is selected
    if (this.cloakId == null) return true;

    const selectedCloak = this.dataSources.find((dataSource) => (dataSource.cloak.id === this.cloakId));
    if (selectedCloak != null) return true;

    // We have a cloak, but it is not online
    return false;
  }

  errorMessage() {
    if (this.dataSources.length === 0 && this.cloakId === null) {
      return "There are no cloaks connected.";
    }

    if (!this.selectedCloakOnline()) {
      return `The cloak ${this.cloakId} is not connected.`;
    }

    return null;
  }

  setDataSourceToken(dataSourceToken) {
    if (this.dataSourceToken === dataSourceToken) return this;
    return this.transform(
      (newSettings) => {
        Object.assign(newSettings, {dataSourceToken, tables: new Set([])});
      }
    );
  }

  selectTable(tableId) {
    return this.transform((newSettings) => newSettings.tables.add(tableId));
  }

  unselectTable(tableId) {
    return this.transform((newSettings) => newSettings.tables.delete(tableId));
  }

  changeTaskName(newName) {
    /* eslint-disable no-param-reassign */
    /* FIXME make all clients of these updating functions operate on a new copy of the settings */
    /* FIXME fix and remove this eslint skip */
    return this.transform((newSettings) => { newSettings.taskName = newName; });
    /* eslint-enable no-param-reassign */
  }

  transform(fun) {
    const newSettings = Object.assign(new SettingsModel(), this);
    fun(newSettings);
    return newSettings;
  }
}

export const SettingsView = (props) =>
  <div>
    <Error{...props} />
    <Form {...props} />
  </div>;

const Error = (props) => {
  const errorMessage = props.settings.errorMessage();
  if (errorMessage) {
    return <div className="alert alert-danger">{errorMessage}</div>;
  } else {
    return null;
  }
};

Error.propTypes = {
  settings: React.PropTypes.instanceOf(SettingsModel).isRequired,
};

const Form = (props) =>
  <form className="form-horizontal">
    <TaskNameControl {...props} />
    <DataSources {...props} />
    <Tables {...props} />
  </form>;

class TaskNameControl extends React.Component {
  constructor(props) {
    super(props);
    this.handleNameChange = this.handleNameChange.bind(this);
  }

  handleNameChange(event) {
    this.props.onChange(this.props.settings.changeTaskName(event.target.value));
  }

  render() {
    return (
      <Control label="Task name">
        <input
          type="text"
          className="form-control"
          onChange={this.handleNameChange}
          value={this.props.settings.taskName}
        />
      </Control>
    );
  }
}

TaskNameControl.propTypes = {
  onChange: React.PropTypes.func.isRequired,
  settings: React.PropTypes.instanceOf(SettingsModel),
};

const Control = (props) =>
  <div className="form-group">
    <label className="col-sm-4 control-label">{props.label}</label>
    <div className="col-sm-6">{props.children}</div>
  </div>;

Control.propTypes = {
  label: React.PropTypes.string.isRequired,
  children: React.PropTypes.node,
};

class DataSources extends React.Component {
  constructor(props) {
    super(props);
    this.onDataSourceSelected = this.onDataSourceSelected.bind(this);
  }

  onDataSourceSelected(event) {
    this.props.onChange(this.props.settings.setDataSourceToken(
      (event.target.value === "__select") ? null : event.target.value));
  }

  render() {
    if (! this.props.settings.hasDataSourcesAvailable()) {
      return null;
    }
    const dataSources =
      [{token: "__select", display: "select a data source", tables: [], cloak: {}}].
        concat(this.props.settings.dataSources);

    return (
      <Control label="Data source">
        <select
          className="form-control"
          value={`${this.props.settings.selectedDataSourceToken()}`}
          onChange={this.onDataSourceSelected}
        >
          {dataSources.map((dataSource) =>
            <option key={dataSource.token} value={dataSource.token}>
              {dataSource.display}
            </option>)}
        </select>
      </Control>
    );
  }
}

DataSources.propTypes = {
  settings: React.PropTypes.instanceOf(SettingsModel).isRequired,
  onChange: React.PropTypes.func.isRequired,
};

class Tables extends React.Component {
  constructor(props) {
    super(props);
    this.onTableSelected = this.onTableSelected.bind(this);
  }

  onTableSelected(event) {
    const newSettings =
      event.target.checked ?
        this.props.settings.selectTable(event.target.value) :
        this.props.settings.unselectTable(event.target.value);
    this.props.onChange(newSettings);
  }

  render() {
    const tables = this.props.settings.selectedDataSourceTables();
    if (this.props.settings.hasDataSourcesAvailable() && tables.length > 0) {
      return (
        <Control label="Tables">
          <div className="table-control">
            {tables.map((table) =>
              <label key={table.id}>
                <input
                  type="checkbox"
                  onChange={this.onTableSelected}
                  checked={this.props.settings.isTableSelected(table)}
                  value={table.id}
                />
                {table.id}
              </label>)}
          </div>
        </Control>
      );
    } else {
      return null;
    }
  }
}

Tables.propTypes = {
  settings: React.PropTypes.instanceOf(SettingsModel).isRequired,
  onChange: React.PropTypes.func.isRequired,
};
