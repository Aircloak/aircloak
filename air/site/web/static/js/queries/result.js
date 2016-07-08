import React from "react";
import _ from "lodash";

import Plotly from "../plotly.js";
import {CodeEditor} from "../code_editor";
import {Info} from "./info";

export class Result extends React.Component {
  constructor(props) {
    super(props);

    this.minRowsToShow = 10;

    this.state = {
      rowsToShowCount: this.minRowsToShow,

      showChart: false,
      chartYAxisIndex: props.columns.length - 1,
    };

    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.renderOptionMenu = this.renderOptionMenu.bind(this);

    this.conditionallyRenderChart = this.conditionallyRenderChart.bind(this);
    this.setChartDataOnRef = this.setChartDataOnRef.bind(this);
    this.plotChart = this.plotChart.bind(this);

    this.showingAllOfFewRows = this.showingAllOfFewRows.bind(this);
    this.showingAllOfManyRows = this.showingAllOfManyRows.bind(this);
    this.showingMinimumNumberOfManyRows = this.showingMinimumNumberOfManyRows.bind(this);
  }

  componentDidUpdate() {
    this.plotChart();
  }

  setChartDataOnRef(ref) {
    this.chartRef = ref;
    this.plotChart();
  }

  plotChart() {
    if (! this.state.showChart || ! this.chartRef) {
      return;
    }
    const chartData = [{
      type: "bar",
      name: this.props.columns[this.state.chartYAxisIndex],
      y: this.props.rows.map((accumulateRow) => accumulateRow.row[this.state.chartYAxisIndex]),
      x: this.props.rows.map((accumulateRow) => {
        const firstPart = _.take(accumulateRow.row, this.state.chartYAxisIndex);
        let secondPart = [];
        if (this.state.chartYAxisIndex < accumulateRow.row.length - 1) {
          secondPart = _.slice(accumulateRow.row, this.state.chartYAxisIndex + 1);
        }
        return _.join(firstPart.concat(secondPart), ", ");
      }),
    }];
    const layout = {
      showlegend: true,
    };
    const displayOptions = {
      staticPlot: true,
    };
    Plotly.newPlot(this.chartRef, chartData, layout, displayOptions);
  }

  handleClickMoreRows() {
    this.setState({rowsToShowCount: Math.min(this.state.rowsToShowCount * 2, this.props.row_count)});
  }

  handleClickLessRows() {
    const rowsToShowCount = Math.max(Math.round(this.state.rowsToShowCount / 2), this.minRowsToShow);
    this.setState({rowsToShowCount});
  }

  showingAllOfFewRows() {
    return this.props.row_count < this.minRowsToShow;
  }

  showingAllOfManyRows() {
    return this.props.row_count === this.state.rowsToShowCount;
  }

  showingMinimumNumberOfManyRows() {
    return this.state.rowsToShowCount === this.minRowsToShow && this.props.row_count > this.minRowsToShow;
  }

  formatValue(value) {
    return isNaN(value) ? value : Math.round(value * 1000) / 1000; // keep 3 decimals at most
  }

  conditionallyRenderChart() {
    if (this.state.showChart) {
      return (
        <div>
          <div className="column-select">
            Use&nbsp;
            <select
              value={this.state.chartYAxisIndex}
              onChange={(e) => {
                this.setState({chartYAxisIndex: parseInt(e.target.value, 10)});
              }}
            >
            {this.props.columns.map((column, i) => <option key={i} value={i}>{column}</option>)}
            </select>&nbsp;
            as value for y-axis.
          </div>

          <div
            ref={this.setChartDataOnRef}
            className="plotlyGraph" style={{width: "100%", height: "500px"}}
          />
        </div>
      );
    } else {
      return null;
    }
  }

  renderRows() {
    let remainingRowsToProduce = this.state.rowsToShowCount;
    return _.flatMap(this.props.rows, (accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(remainingRowsToProduce, accumulateRow.occurrences);
      return _.range(occurrencesForAccumulateRow).map((occurrenceCount) => {
        remainingRowsToProduce -= 1;
        return (<tr key={`${i}-${occurrenceCount}`}>
          {accumulateRow.row.map((value, j) => <td key={j}>{this.formatValue(value)}</td>)}
        </tr>);
      });
    });
  }

  renderShowAll() {
    if (this.showingAllOfFewRows()) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.
        </div>
      );
    } else if (this.showingAllOfManyRows()) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickLessRows}>Show fewer rows</a>
        </div>
      );
    } else if (this.showingMinimumNumberOfManyRows()) {
      return (
        <div className="row-count">
          Showing {this.minRowsToShow} of {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickMoreRows}>Show more rows</a>
        </div>
      );
    } else {
      const rowsShown = Math.min(this.state.rowsToShowCount, this.props.row_count);
      return (
        <div className="row-count">
          Showing {rowsShown} of {this.props.row_count} rows.&nbsp;
          Show&nbsp;
          <a onClick={this.handleClickLessRows}>fewer rows</a>,&nbsp;
          <a onClick={this.handleClickMoreRows}>more rows</a>
        </div>
      );
    }
  }

  renderOptionMenu() {
    const chartButtonText = this.state.showChart ? "HideChart" : "Show chart";
    return (
      <div className="options-menu">
        <button
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChart: ! this.state.showChart})}
        >
          {chartButtonText}
        </button>
      </div>
    );
  }

  render() {
    return (
      <div className="panel panel-success">
        <div className="panel-heading" />
        <div className="panel-body">
          <CodeEditor
            onRun={() => {}}
            onSave={() => {}}
            onChange={() => {}}
            statement={this.props.statement}
            readOnly
          />
          <Info info={this.props.info} />
          <table className="table table-striped table-hover">
            <thead>
              <tr>
                {this.props.columns.map((column, i) =>
                  <th key={i}>{column}</th>
                )}
              </tr>
            </thead>

            <tbody>
              {this.renderRows()}
            </tbody>
          </table>
          {this.renderShowAll()}
          {this.renderOptionMenu()}
          {this.conditionallyRenderChart()}
        </div>
      </div>
    );
  }

}

Result.propTypes = {
  statement: React.PropTypes.string,
  columns: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  rows: React.PropTypes.arrayOf(React.PropTypes.shape({
    occurrences: React.PropTypes.integer,
    row: React.PropTypes.array,
  })).isRequired,
  row_count: React.PropTypes.number,
  info: Info.propTypes.info,
};
