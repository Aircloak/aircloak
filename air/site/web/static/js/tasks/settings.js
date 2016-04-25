import React from "react"

export class SettingsView extends React.Component {
  // ----------------------------------------------------------------
  // View helpers
  // ----------------------------------------------------------------

  cloakConnected() {
    if (this.props.settings.cloakId == null) return true; // no cloak selected, so we're fine
    return (this.props.settings.dataSources.find((dataSource) => {
        return dataSource.cloak.id == this.props.settings.cloakId}));
  }

  dataSources(settings) {
    return [{token: "__select", display: "select a data source", tables: [], cloak: {}}].concat(
        this.props.settings.dataSources);
  }

  selectedDataSource(settings) {
    return (
          this.dataSources(settings).find(
                (dataSource) => {return dataSource.token == settings.dataSourceToken}
              ) ||
          this.dataSources(settings)[0]
        );
  }


  // ----------------------------------------------------------------
  // Event handlers
  // ----------------------------------------------------------------

  onDataSourceSelected(event) {
    this.fireChange({
          dataSourceToken: event.target.value,
          tables: new Set([])
        });
  }

  onTableSelectionChanged(event) {
    let tables = new Set(this.props.settings.tables);
    if (event.target.checked)
      tables.add(event.target.value);
    else
      tables.delete(event.target.value);

    this.fireChange({tables: tables});
  }

  fireChange(settingUpdates) {
    let newSettings = Object.assign({}, this.props.settings, settingUpdates),
        dataSource = this.selectedDataSource(newSettings);

    _.merge(newSettings, {
          dataSourceToken: dataSource ? dataSource.token : null,
        });

    if (newSettings.dataSourceToken == "__select") newSettings.dataSourceToken = null;
    this.props.onChange(newSettings);
  }


  // ----------------------------------------------------------------
  // Render functions
  // ----------------------------------------------------------------

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
      return this.renderReadOnly("There are no cloaks connected.");
    else if (this.cloakConnected())
      return this.renderForm();
    else
      return (
            <div>
              {this.renderReadOnly(`The cloak ${this.props.settings.cloakId} is not connected.`)}
              {this.renderForm()}
            </div>
          );
  }

  renderReadOnly(reason) {
    return (
          <div className="alert alert-danger">
            {reason}.
          </div>
        );
  }

  renderForm() {
    return (
          <form className="form-horizontal">
            {this.renderControl("Data source", this.renderDataSourceSelection.bind(this))}
            {this.renderControl("Tables", this.renderTablesSelection.bind(this))}
          </form>
        );
  }

  renderControl(label, renderFun) {
    return (
          <div className="form-group">
            <label className="col-sm-2 control-label">{label}</label>
            <div className="col-sm-8">{renderFun()}</div>
          </div>
        );
  }

  renderDataSourceSelection() {
    return (
          <select
            className="form-control"
            value={`${this.selectedDataSource(this.props.settings).token}`}
            onChange={this.onDataSourceSelected.bind(this)}
          >
            {this.dataSources(this.props.settings).map((dataSource) =>
                <option key={dataSource.token} value={dataSource.token}>
                  {dataSource.display}
                </option>)}
          </select>
        );
  }

  renderTablesSelection() {
    return (
          <div style={{'paddingTop': '7px'}}>
            {this.selectedDataSource(this.props.settings).tables.map((table, index) =>
                <div key={table.id}>
                  <input type="checkbox"
                    onChange={this.onTableSelectionChanged.bind(this)}
                    checked={this.props.settings.tables.has(table.id)}
                    value={table.id} />
                  {table.id}
                </div>)}
          </div>
        );
  }
}
