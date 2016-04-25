import React from "react"

export class SettingsView extends React.Component {
  // ----------------------------------------------------------------
  // View helpers
  // ----------------------------------------------------------------

  cloaks() {
    return [{id: "__select", name: "select a cloak", data_sources: []}].concat(this.props.cloaks);
  }

  cloakConnected() {
    let allCloaks = this.cloaks();
    if (this.props.settings.cloakId == null) return true; // no cloak selected, so we're fine
    return (this.cloaks().find((cloak) => {return cloak.id == this.props.settings.cloakId}));
  }

  selectedCloakDataSources(settings) {
    return [{id: "__select", display: "select a data source", tables: []}].concat(
        this.selectedCloak(settings).data_sources);
  }

  selectedCloak(settings) {
    return (
          this.cloaks().find((cloak) => {return cloak.id == settings.cloakId}) ||
          this.cloaks()[0]
        );
  }

  selectedDataSource(settings) {
    return (
          this.selectedCloakDataSources(settings).find(
                (dataSource) => {return dataSource.id == settings.dataSource}
              ) ||
          this.selectedCloakDataSources(settings)[0]
        );
  }

  // Finds the selected element, or returns the first one if it's the only one in the list.
  findSelected(elements, id) {
    if (elements.length == 1 && id == null) return elements[0];
    return elements.find((el) => {return el.id == id});
  }

  selectedDataSourceTables(settings) {
    return this.selectedDataSource(settings).tables;
  }

  tableSelected(tableId) {
    return this.props.settings.tables.has(tableId);
  }


  // ----------------------------------------------------------------
  // Event handlers
  // ----------------------------------------------------------------

  onCloakSelected(event) {
    this.fireChange({
          cloakId: event.target.value,
          dataSource: null,
          tables: new Set([])
        });
  }

  onDataSourceSelected(event) {
    this.fireChange({
          dataSource: event.target.value,
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
        cloak = this.selectedCloak(newSettings),
        dataSource = this.selectedDataSource(newSettings);

    _.merge(newSettings, {
          cloakId: cloak.id,
          dataSource: dataSource ? dataSource.id : null,
        });

    if (newSettings.cloakId == "__select") newSettings.cloakId = null;
    if (newSettings.dataSource == "__select") newSettings.dataSource = null;
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
    if (this.cloaks().length == 1 && this.props.settings.cloakId == null)
      return this.renderReadOnly("There are no cloaks connected.")
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
            {this.renderControl("Cloak", this.renderCloakSelection.bind(this))}
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

  renderCloakSelection() {
    return(
          <select
            className="form-control"
            value={this.props.settings.cloakId || ""}
            onChange={this.onCloakSelected.bind(this)}
          >
            {this.cloaks().map((cloak) =>
                <option key={cloak.id} value={cloak.id}>
                  {cloak.name}
                </option>
              )}
          </select>
        );
  }

  renderDataSourceSelection() {
    return (
          <select
            className="form-control"
            value={this.props.settings.dataSource || ""}
            onChange={this.onDataSourceSelected.bind(this)}
          >
            {this.selectedCloakDataSources(this.props.settings).map((dataSource) =>
                <option key={dataSource.id} value={dataSource.id}>
                  {dataSource.display || dataSource.id}
                </option>)}
          </select>
        );
  }

  renderTablesSelection() {
    return (
          <div style={{'paddingTop': '7px'}}>
            {this.selectedDataSourceTables(this.props.settings).map((table, index) =>
                <div key={table.id}>
                  <input type="checkbox"
                    onChange={this.onTableSelectionChanged.bind(this)}
                    checked={this.tableSelected(table.id)}
                    value={table.id} />
                  {table.id}
                </div>)}
          </div>
        );
  }
}
