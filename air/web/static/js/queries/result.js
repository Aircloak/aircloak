// @flow

import React from "react";
import _ from "lodash";
import {Bar} from "react-chartjs-2";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";
import {GraphData} from "./graph_data";
import type {GraphDataT} from "./graph_data";
import type {Error} from "./error";

export type Row = {
  occurrences: number,
  row: any[],
};

export type Column = string;

export type Result = {
  id: string,
  statement: string,
  columns: Column[],
  rows: Row[],
  row_count: number,
  info: string[],
  pendingResult: boolean,
  error: Error,
};

export class ResultView extends React.Component {
  constructor(props: Result) {
    super(props);

    this.minRowsToShow = 10;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
    };

    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.renderOptionMenu = this.renderOptionMenu.bind(this);

    this.conditionallyRenderChart = this.conditionallyRenderChart.bind(this);
    this.formatValue = this.formatValue.bind(this);

    this.showingAllOfFewRows = this.showingAllOfFewRows.bind(this);
    this.showingAllOfManyRows = this.showingAllOfManyRows.bind(this);
    this.showingMinimumNumberOfManyRows = this.showingMinimumNumberOfManyRows.bind(this);

    this.graphData = new GraphData(this.props.rows, this.props.columns, this.formatValue);
  }

  state: {rowsToShowCount: number, showChart: boolean};
  props: Result;
  minRowsToShow: number;
  graphData: GraphDataT;
  formatValue: (value: any) => string | number;
  handleClickMoreRows: () => void;
  handleClickLessRows: () => void;
  renderRows: () => void;
  renderShowAll: () => void;
  renderOptionMenu: () => void;
  conditionallyRenderChart: () => void;
  showingAllOfFewRows: () => void;
  showingAllOfManyRows: () => void;
  showingMinimumNumberOfManyRows: () => void;

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

  formatValue(value: any): number | string {
    if (value === null) {
      return "<null>";
    } else if (this.isNumeric(value)) {
      return Math.round(value * 1000) / 1000; // keep 3 decimals at most
    } else {
      return value.toString();
    }
  }

  isNumeric(n: any) {
    return typeof(n) === "number" && isFinite(n);
  }

  conditionallyRenderChart() {
    if (this.state.showChart) {
      const data = this.graphData.traces("bar")[0];

      return (
        <Bar
          data={{
            labels: data.x,
            datasets: [
              {
                label: data.name,
                data: data.y,
              },
            ],
          }}

          options={{
            scales: {
              xAxes: [{
                scaleLabel: {
                  display: true,
                  labelString: this.graphData.xAxisLabel(),
                },
              }],
            },
          }}

          width={714}
          height={600}
        />
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

  renderChartButton() {
    if (this.graphData.charteable()) {
      const chartButtonText = this.state.showChart ? "Hide chart" : "Show chart";
      return (
        <button
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChart: ! this.state.showChart})}
        >
          {chartButtonText}
        </button>
      );
    } else {
      return null;
    }
  }

  renderOptionMenu() {
    return (
      <div className="options-menu">
        <a className="btn btn-default btn-xs" href={`/queries/${this.props.id}.csv`}>Download as CSV</a>
        &nbsp;
        {this.renderChartButton()}
      </div>
    );
  }

  render() {
    return (
      <div className="panel panel-success">
        <div className="panel-heading" />
        <div className="panel-body">
          <CodeViewer statement={this.props.statement} />
          <Info info={this.props.info} />
          <div className="result-table">
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
          </div>
          {this.renderShowAll()}
          {this.renderOptionMenu()}
          {this.conditionallyRenderChart()}
        </div>
      </div>
    );
  }

}
