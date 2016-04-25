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

  isCloakConnected() {
    if (this.cloakId == null) return true; // no cloak selected, so we're fine
    let cloak = (this.dataSources.find((dataSource) => {return dataSource.cloak.id == this.cloakId}));
    return (cloak != null);
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
            {this.renderEditor()}
          </div>
        );
  }

  renderEditor() {
    if (this.props.settings.dataSources.length == 0 && this.props.settings.cloakId == null)
      return <Error reason="There are no cloaks connected." />;
    else if (!this.props.settings.isCloakConnected())
      return (
            <div>
              <Error reason={`The cloak ${this.props.settings.cloakId} is not connected.`} />
              <Form {...this.props} />
            </div>
          );
    else
      return <Form {...this.props} />;
  }
}

class Error extends React.Component {
  render() {
    return <div className="alert alert-danger">{this.props.reason}.</div>;
  }
}

class Form extends React.Component {
  render() {
    return (
          <form className="form-horizontal">
            <Control label="Data source" component={<DataSources {...this.props} />} />
            <Control label="Tables" component={<Tables {...this.props} />} />
          </form>
        );
  }
}

class Control extends React.Component {
  render() {
    return (
          <div className="form-group">
            <label className="col-sm-2 control-label">{this.props.label}</label>
            <div className="col-sm-8">{this.props.component}</div>
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
