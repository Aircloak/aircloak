import React from "react"

export class SettingsModel {
  constructor(props) {
    Object.assign(this, props);
  }

  selectedDataSource() {
    return this.dataSources.find((dataSource) => {return dataSource.token == this.dataSourceToken});
  }

  selectedDataSourceToken() {
    let selectedDataSource = this.selectedDataSource();
    return selectedDataSource ? selectedDataSource.token : null;
  }

  selectedDataSourceTables() {
    let selectedDataSource = this.selectedDataSource();
    return selectedDataSource ? selectedDataSource.tables : [];
  }

  isTableSelected(table) {
    return this.tables.has(table.id)
  }

  hasAssignedDataSource() {
    return this.dataSourceToken !== null;
  }

  hasAssignedTables() {
    return this.tables.size > 0;
  }

  // Returns false if and only if the task has been assigned
  // a cloak, and this cloak is not online. That is, it returns
  // true if the task has not yet been assigned a cloak.
  selectedCloakOnline() {
    // some cloaks are connected, but no cloak is selected
    if (this.cloakId == null) return true;

    let selectedCloak = (this.dataSources.find((dataSource) => {return dataSource.cloak.id == this.cloakId}));
    if (selectedCloak != null) return true;

    // We have a cloak, but it is not online
    return false;
  }

  errorMessage() {
    if (this.dataSources.length == 0 && this.cloakId == null) {
      return "There are no cloaks connected.";
    }

    if (!this.selectedCloakOnline()) {
      return `The cloak ${this.cloakId} is not connected.`;
    }
  }

  setDataSourceToken(dataSourceToken) {
    if (this.dataSourceToken == dataSourceToken) return this;
    return this.transform(
          (newSettings) => {
            Object.assign(newSettings, {dataSourceToken: dataSourceToken, tables: new Set([])})
          }
        )
  }

  selectTable(tableId) {
    return this.transform((newSettings) => {newSettings.tables.add(tableId)});
  }

  unselectTable(tableId) {
    return this.transform((newSettings) => {newSettings.tables.delete(tableId)});
  }

  changeTaskName(newName) {
    return this.transform((newSettings) => {newSettings.taskName = newName});
  }

  transform(fun) {
    let newSettings = Object.assign(new SettingsModel(), this);
    fun(newSettings);
    return newSettings;
  }
}

export class SettingsView extends React.Component {
  render() {
    return (
          <div>
            <Error{...this.props} />
            <Form {...this.props} />
          </div>
        );
  }
}

class Error extends React.Component {
  render() {
    let errorMessage = this.props.settings.errorMessage();
    if (errorMessage)
      return <div className="alert alert-danger">{errorMessage}</div>;
    else
      return null;
  }
}

class Form extends React.Component {
  render() {
    if (this.props.settings.dataSources.length > 0) {
      return (
            <form className="form-horizontal">
              <TaskNameControl {...this.props} />
              <DataSources {...this.props} />
              <Tables {...this.props} />
            </form>
          );
    }
    else
      return null;
  }
}

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
          <input type="text" className="form-control"
              onChange={this.handleNameChange}
              value={this.props.settings.taskName} />
        </Control>
    );
  }
}

class Control extends React.Component {
  render() {
    return (
          <div className="form-group">
            <label className="col-sm-4 control-label">{this.props.label}</label>
            <div className="col-sm-6">{this.props.children}</div>
          </div>
        );
  }
}

class DataSources extends React.Component {
  onDataSourceSelected(event) {
    this.props.onChange(this.props.settings.setDataSourceToken(
        (event.target.value == "__select") ? null : event.target.value))
  }

  render() {
    let dataSources =
      [{token: "__select", display: "select a data source", tables: [], cloak: {}}].
          concat(this.props.settings.dataSources)

    return (
          <Control label="Data source">
            <select
              className="form-control"
              value={`${this.props.settings.selectedDataSourceToken()}`}
              onChange={this.onDataSourceSelected.bind(this)}
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

class Tables extends React.Component {
  onTableSelected(event) {
    let newSettings =
      event.target.checked ?
          this.props.settings.selectTable(event.target.value) :
          this.props.settings.unselectTable(event.target.value);
    this.props.onChange(newSettings);
  }

  render() {
    let tables = this.props.settings.selectedDataSourceTables();
    if (tables.length > 0) {
      return (
            <Control label="Tables">
              <div style={{paddingTop: '7px'}}>
                {tables.map((table) =>
                    <label key={table.id}>
                      <input style={{marginRight: '5px'}} type="checkbox"
                        onChange={this.onTableSelected.bind(this)}
                        checked={this.props.settings.isTableSelected(table)}
                        value={table.id} />
                      {table.id}
                    </label>)}
              </div>
            </Control>
          );
    }
    else
      return null;
  }
}
