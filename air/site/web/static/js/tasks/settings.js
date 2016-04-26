import React from "react"

export class SettingsModel {
  constructor(props) {
    Object.assign(this, props);
  }

  selectedDataSourceToken() {
    let selectedDataSource = this.dataSources.find(
        (dataSource) => {return dataSource.token == this.dataSourceToken});

    return selectedDataSource ? selectedDataSource.token : null;
  }

  selectedDataSourceTables() {
    let selectedDataSource = this.dataSources.find(
        (dataSource) => {return dataSource.token == this.dataSourceToken});

    return selectedDataSource ? selectedDataSource.tables : [];
  }

  isTableSelected(table) {
    return this.tables.has(table.id)
  }

  errorMessage() {
    if (this.dataSources.length == 0 && this.cloakId == null)
      return "There are no cloaks connected.";

    if (this.cloakId == null) return null; // some cloaks are connected, but no cloak is selected

    let selectedCloak = (this.dataSources.find((dataSource) => {return dataSource.cloak.id == this.cloakId}));
    if (selectedCloak != null) return null;

    return `The cloak ${this.cloakId} is not connected.`;
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
            <h1>Settings view</h1>
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
    if (this.props.settings.dataSources.length > 0)
      return (
            <form className="form-horizontal">
              <Control label="Data source">
                <DataSources {...this.props} />
              </Control>

              <Control label="Tables">
                <Tables {...this.props} />
              </Control>
            </form>
          );
    else
      return null;
  }
}

class Control extends React.Component {
  render() {
    return (
          <div className="form-group">
            <label className="col-sm-2 control-label">{this.props.label}</label>
            <div className="col-sm-8">{this.props.children}</div>
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

    return(
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
        )
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
    return (
          <div style={{'paddingTop': '7px'}}>
            {this.props.settings.selectedDataSourceTables().map((table) =>
                <div key={table.id}>
                  <input type="checkbox"
                    onChange={this.onTableSelected.bind(this)}
                    checked={this.props.settings.isTableSelected(table)}
                    value={table.id} />
                  {table.id}
                </div>)}
          </div>
        );
  }
}
