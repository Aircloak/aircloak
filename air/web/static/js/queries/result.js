// @flow

import React from "react";
import _ from "lodash";
import $ from "jquery";

import {CodeViewer} from "../code_viewer";
import {Info} from "./info";
import {GraphData, GraphInfo, GraphConfig} from "./graph_data";
import {GraphConfigView} from "./graph_config_view";
import {GraphView} from "./graph_view";
import type {GraphDataT, GraphInfoT} from "./graph_data";
import {TableAligner} from "./table_aligner";
import type {TableAlignerT} from "./table_aligner";
import {loadBuckets} from "../request";

export type Row = {
  occurrences: number,
  row: any[],
  users_count: number,
};

export type Column = string;

export type Result = {
  id: string,
  statement: string,
  columns: Column[],
  rows: Row[],
  row_count: number,
  info: string[],
  error: string,
  query_state: string,
  data_source: {
    name: string,
  },
  user: {
    name: string,
  },
  inserted_at: string,
  session_id: string,
};

type State = {
  rowsToShowCount: number,
  showChart: boolean,
  showChartConfig: boolean,
  graphConfig: GraphConfig,
  tableAligner: TableAlignerT,
  availableRows: Row[],
};

const UNRELIABLE_USER_COUNT_THRESHOLD = 15;
const ZERO_WIDTH_SPACE = "\u200B";

export class ResultView extends React.Component {
  constructor(props: Result) {
    super(props);

    this.minRowsToShow = 10;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
      showChartConfig: true,
      graphConfig: new GraphConfig(),
      tableAligner: new TableAligner(props.rows),
      availableRows: this.props.rows,
    };

    this.componentDidUpdate = this.componentDidUpdate.bind(this);

    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.renderChartButton = this.renderChartButton.bind(this);
    this.renderAxesButton = this.renderAxesButton.bind(this);
    this.renderOptionMenu = this.renderOptionMenu.bind(this);

    this.conditionallyRenderChart = this.conditionallyRenderChart.bind(this);
    this.conditionallyRenderChartConfig = this.conditionallyRenderChartConfig.bind(this);
    this.formatValue = this.formatValue.bind(this);

    this.showingAllOfFewRows = this.showingAllOfFewRows.bind(this);
    this.showingAllOfManyRows = this.showingAllOfManyRows.bind(this);
    this.showingMinimumNumberOfManyRows = this.showingMinimumNumberOfManyRows.bind(this);

    this.graphInfo = new GraphInfo(this.props.columns, this.state.availableRows);
    this.rebuildGraphData();

    this.addX = this.addX.bind(this);
    this.addY = this.addY.bind(this);
    this.removeColumn = this.removeColumn.bind(this);
  }

  state: State;
  props: Result;
  minRowsToShow: number;
  graphData: GraphDataT;
  graphInfo: GraphInfoT;
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
  componentDidUpdate: (prevProps: Result, prevState: State) => void;
  renderChartButton: () => void;
  renderAxesButton: () => void;
  conditionallyRenderChartConfig: () => void;
  rebuildGraphData: () => void;
  addX: (col: number) => () => void;
  addY: (col: number) => () => void;
  removeColumn: (col: number) => () => void;

  componentDidUpdate() {
    this.rebuildGraphData();
  }

  rebuildGraphData() {
    this.graphData = new GraphData(
      this.props.columns,
      this.state.availableRows,
      this.state.graphConfig,
      this.formatValue
    );
  }

  handleClickMoreRows() {
    const increment = Math.min(this.state.rowsToShowCount, 100);
    const rowsToShowCount = Math.min(this.state.rowsToShowCount + increment, this.props.row_count);
    const availableRowsCount = _.sum(_.flatMap(this.state.availableRows, (row) => row.occurrences));
    if (rowsToShowCount <= availableRowsCount) {
      this.setState({rowsToShowCount: Math.min(this.state.rowsToShowCount * 2, this.props.row_count)});
    } else {
      loadBuckets(this.props.id, availableRowsCount, this.context.authentication, {
        success: (newRows) => {
          this.setState({
            availableRows: _.concat(this.state.availableRows, newRows),
            rowsToShowCount: Math.min(this.state.rowsToShowCount * 2, this.props.row_count),
          });
        },
      });
    }
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

  addX(col: number) {
    return () => this.setState({graphConfig: this.state.graphConfig.addX(col)});
  }

  addY(col: number) {
    return () => this.setState({graphConfig: this.state.graphConfig.addY(col)});
  }

  removeColumn(col: number) {
    return () => this.setState({graphConfig: this.state.graphConfig.remove(col)});
  }

  formatValue(value: any): number | string {
    if (value === null) {
      return "<null>";
    } else if (this.isNumeric(value)) {
      return Math.round(value * 1000) / 1000; // keep 3 decimals at most
    } else if (value === "") {
      return ZERO_WIDTH_SPACE; // keeps table row from collapsing
    } else {
      return value.toString();
    }
  }

  isNumeric(n: any) {
    return typeof(n) === "number" && isFinite(n);
  }

  conditionallyRenderChart() {
    if (this.state.showChart) {
      return (
        <GraphView
          graphData={this.graphData}
          width={714}
          height={600}
        />
      );
    } else {
      return null;
    }
  }

  conditionallyRenderChartConfig() {
    if (this.state.showChart && this.state.showChartConfig) {
      return (
        <GraphConfigView
          graphInfo={this.graphInfo}
          graphConfig={this.state.graphConfig}
          addX={this.addX}
          addY={this.addY}
          remove={this.removeColumn}
        />
      );
    } else {
      return null;
    }
  }

  getRowAttrs(row: Row) {
    if (row.users_count < UNRELIABLE_USER_COUNT_THRESHOLD) {
      return {
        title: "These values are unreliable because of the low number of users involved.",
        "data-toggle": "tooltip",
        className: "unreliable",
      };
    } else {
      return {};
    }
  }

  renderRows() {
    let remainingRowsToProduce = this.state.rowsToShowCount;
    const rows = _.flatMap(this.state.availableRows, (accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(remainingRowsToProduce, accumulateRow.occurrences);
      return _.range(occurrencesForAccumulateRow).map((occurrenceCount) => {
        remainingRowsToProduce -= 1;
        return (<tr key={`${i}-${occurrenceCount}`} {...this.getRowAttrs(accumulateRow)}>
          {accumulateRow.row.map((value, j) =>
            <td key={j} className={this.state.tableAligner.alignmentClass(j)}>
              {this.formatValue(value)}
            </td>
          )}
        </tr>);
      });
    });
    $("tr[data-toggle='tooltip']").tooltip();
    return rows;
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
    if (this.graphInfo.chartable()) {
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

  renderAxesButton() {
    if (this.state.showChart) {
      const text = this.state.showChartConfig ? "Hide axes" : "Show axes";
      return (
        <button
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChartConfig: ! this.state.showChartConfig})}
        >
          {text}
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
        &nbsp;
        {this.renderAxesButton()}
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
            <table className="table table-striped table-condensed table-hover">
              <thead>
                <tr>
                  {this.props.columns.map((column, i) =>
                    <th key={i} className={this.state.tableAligner.alignmentClass(i)}>{column}</th>
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
          {this.conditionallyRenderChartConfig()}
          {this.conditionallyRenderChart()}
        </div>
      </div>
    );
  }

}

ResultView.contextTypes = {
  authentication: React.PropTypes.object.isRequired,
};
