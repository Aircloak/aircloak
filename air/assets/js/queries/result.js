// @flow

import type { Node, Element } from "React";
import type { AuthContextType } from "../authentication_provider";
import React from "react";
import { AuthContext } from "../authentication_provider";
import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import QueryNote from "./query_note";
import { GraphData, GraphInfo, GraphConfig } from "./graph_data";
import GraphConfigView from "./graph_config_view";
import GraphView from "./graph_view";
import type { GraphDataT, GraphInfoT } from "./graph_data";
import { TableAligner } from "./table_aligner";
import type { TableAlignerT } from "./table_aligner";
import type { NumberFormat } from "../number_format";
import { formatNumber } from "../number_format";
import { loadBuckets } from "../request";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import NoteButton from "./note_button";
import ResultTime from "./result_time";
import activateTooltips from "../tooltips";
import loader from "../../static/images/loader.gif";

export type Row = {
  occurrences: number,
  row: any[],
  unreliable: boolean,
};

export type Column = string;
export type Type = string;

type CommonResultFeatures = {
  id: string,
  statement: string,
  note: string | null,
  data_source: {
    name: string,
  },
  private_permalink: ?string,
  public_permalink: ?string,
  session_id: ?string,
  inserted_at: string | number,
};

export type SuccessResult = CommonResultFeatures & {
  query_state: "completed",
  columns: Column[],
  types: Type[],
  rows: Row[],
  row_count: number,
  info: string[],
  user: {
    name: string,
  },
  buckets_link: string,
};

export type PendingResult = CommonResultFeatures & {
  query_state: "created",
};

export type CancelledResult = CommonResultFeatures & {
  query_state: "cancelled",
};

export type ErrorResult = CommonResultFeatures & {
  query_state: "error",
  error: string,
  info: string[],
};

export type Result =
  | SuccessResult
  | PendingResult
  | CancelledResult
  | ErrorResult;

type Props = {
  result: SuccessResult,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  showDataSource?: boolean,
  onDeleteClick?: (queryId: string) => void,
  updateNote?: (id: string, note: string | null) => void,
};

type State = {
  rowsToShowCount: number,
  showChart: boolean,
  showChartConfig: boolean,
  graphConfig: GraphConfig,
  tableAligner: TableAlignerT,
  availableRows: Row[],
  availableChunks: number,
  loadingChunks: boolean,
  loadError: boolean,
};

const ZERO_WIDTH_SPACE = "\u200B";
const ALL_CHUNKS = -1;

export class ResultView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.minRowsToShow = 10;
    const { result } = props;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
      showChartConfig: true,
      graphConfig: new GraphConfig(),
      tableAligner: TableAligner(result.rows),
      availableRows: result.rows,
      availableChunks: 1,
      loadingChunks: false,
      loadError: false,
    };

    const { availableRows } = this.state;
    this.graphInfo = GraphInfo(result.columns, availableRows);
    this.rebuildGraphData();
  }

  // eslint-disable-next-line react/static-property-placement
  static contextType: React$Context<AuthContextType> = AuthContext;

  minRowsToShow: number;

  graphInfo: GraphInfoT;

  graphData: GraphDataT;

  componentDidUpdate: () => void = () => {
    this.rebuildGraphData();
  };

  rebuildGraphData: () => void = () => {
    const { result } = this.props;
    const { availableRows, graphConfig } = this.state;
    this.graphData = GraphData(
      result.columns,
      availableRows,
      graphConfig,
      this.formatValue
    );
  };

  handleClickMoreRows: () => void = () => {
    const { result } = this.props;
    const { rowsToShowCount, availableRows, availableChunks } = this.state;
    const updatedRowsToShowCount = Math.min(
      2 * rowsToShowCount,
      result.row_count
    );
    this.loadAndShowMoreRows(
      updatedRowsToShowCount,
      availableRows,
      availableChunks
    );
  };

  handleClickLessRows: () => void = () => {
    this.setState((state) => {
      const updatedRowsToShowCount = Math.max(
        Math.round(state.rowsToShowCount / 2),
        this.minRowsToShow
      );
      return { rowsToShowCount: updatedRowsToShowCount };
    });
  };

  showChart: () => void = () => {
    const { availableChunks } = this.state;
    if (availableChunks !== ALL_CHUNKS) {
      this.loadChunks(ALL_CHUNKS, (allRows) => {
        this.setState({
          availableRows: allRows,
          availableChunks: ALL_CHUNKS,
          showChart: true,
        });
      });
    } else {
      this.setState({ showChart: true });
    }
  };

  loadAndShowMoreRows: (
    rowsToShowCount: number,
    availableRows: Array<Row>,
    availableChunks: number
  ) => void = (
    rowsToShowCount: number,
    availableRows: Row[],
    availableChunks: number
  ) => {
    const availableRowsCount = availableRows
      .flatMap((row) => row.occurrences)
      .reduce((a, b) => a + b, 0);

    if (
      availableChunks === ALL_CHUNKS ||
      rowsToShowCount <= availableRowsCount
    ) {
      this.setState({ rowsToShowCount, availableRows, availableChunks });
    } else {
      this.loadChunks(availableChunks, (newRows) => {
        if (newRows.length > 0) {
          const newAvailableRows = availableRows.concat(newRows);
          const newAvailableChunks = availableChunks + 1;
          this.setState({
            rowsToShowCount: availableRowsCount,
            availableRows: newAvailableRows,
            availableChunks: newAvailableChunks,
          });
          this.loadAndShowMoreRows(
            rowsToShowCount,
            newAvailableRows,
            newAvailableChunks
          );
        }
      });
    }
  };

  loadChunks: (
    desiredChunk: number,
    fun: (rows: Array<Row>) => void
  ) => void = (desiredChunk: number, fun: (rows: Row[]) => void) => {
    this.setState({ loadingChunks: true, loadError: false });
    const { result } = this.props;
    const { authentication } = this.context;
    loadBuckets(result.buckets_link, desiredChunk, authentication, {
      success: (buckets) => {
        this.setState({ loadingChunks: false, loadError: false });
        fun(buckets);
      },
      error: () => {
        this.setState({ loadingChunks: false, loadError: true });
      },
    });
  };

  addX: (col: number) => () => void = (col: number) => () =>
    this.setState((state) => ({ graphConfig: state.graphConfig.addX(col) }));

  addY: (col: number) => () => void = (col: number) => () =>
    this.setState((state) => ({ graphConfig: state.graphConfig.addY(col) }));

  removeColumn: (col: number) => () => void = (col: number) => () =>
    this.setState((state) => ({ graphConfig: state.graphConfig.remove(col) }));

  formatValue: (value: any, columnIndex: number) => string = (
    value: any,
    columnIndex: number
  ): string => {
    const { result, numberFormat } = this.props;
    const type = result.types[columnIndex];
    if (value === null) {
      return "<null>";
    } else if (value === "") {
      return ZERO_WIDTH_SPACE; // keeps table row from collapsing
    } else if (this.isNumeric(value)) {
      return formatNumber(value, numberFormat);
    } else if (type === "datetime") {
      return this.formatDateTime(value);
    } else if (type === "time") {
      return this.formatTime(value);
    } else {
      return value.toString();
    }
  };

  isNumeric: (n: any) => boolean = (n: any): boolean =>
    typeof n === "number" && Number.isFinite(n);

  formatDateTime: (value: string) => string = (value: string): string => {
    const [date, time] = value.split("T");
    if (time === undefined) {
      return date;
    } else {
      return `${date} ${this.formatTime(time)}`;
    }
  };

  formatTime: (value: string) => string = (value: string): string => {
    const [hms, us] = value.split(".");
    const ZERO_US = "000000";
    if (us === ZERO_US || us === undefined) {
      return hms;
    } else {
      const SUBSECOND_PRECISION = 3;
      return `${hms}.${us.substr(0, SUBSECOND_PRECISION)}`;
    }
  };

  conditionallyRenderChart: () => null | Node = () => {
    const { showChart } = this.state;
    if (showChart) {
      return <GraphView graphData={this.graphData} width={714} height={600} />;
    } else {
      return null;
    }
  };

  conditionallyRenderChartConfig: () =>
    | null
    | Element<"div">
    | Element<"p">
    | Node = () => {
    const {
      loadingChunks,
      loadError,
      showChart,
      showChartConfig,
      graphConfig,
    } = this.state;
    if (loadingChunks) {
      return (
        <p className="text-center">
          {" "}
          <img alt="indication of more rows being loaded" src={loader} />{" "}
          Loading more rows.
        </p>
      );
    } else if (loadError) {
      return (
        <div className="alert alert-danger">Failed to load more rows.</div>
      );
    } else if (showChart && showChartConfig) {
      return (
        <GraphConfigView
          graphInfo={this.graphInfo}
          graphConfig={graphConfig}
          addX={this.addX}
          addY={this.addY}
          remove={this.removeColumn}
        />
      );
    } else {
      return null;
    }
  };

  getRowAttrs: (
    row: Row
  ) => { className: string, dataToggle: string, title: string } = (
    row: Row
  ) => {
    if (row.unreliable) {
      return {
        title:
          "These values are unreliable because of the low number of users involved.",
        dataToggle: "tooltip",
        className: "unreliable",
      };
    } else {
      return {};
    }
  };

  getInfoMessages: () => Array<string> = (): string[] => {
    const { result, debugModeEnabled } = this.props;
    const messages = result.info;
    if (!debugModeEnabled) {
      return messages.filter((message) => !message.startsWith("[Debug]"));
    } else {
      return messages;
    }
  };

  renderRows: () => Array<Array<Node>> = () => {
    const { rowsToShowCount, availableRows, tableAligner } = this.state;
    let remainingRowsToProduce = rowsToShowCount;
    const rows = availableRows.flatMap<Array<Node>>((accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(
        remainingRowsToProduce,
        accumulateRow.occurrences
      );
      const result: Array<Node> = [];
      for (
        let occurrenceCount = 0;
        occurrenceCount < occurrencesForAccumulateRow;
        occurrenceCount++
      ) {
        remainingRowsToProduce -= 1;
        const { title, dataToggle, className } = this.getRowAttrs(
          accumulateRow
        );
        result.push(
          <tr
            key={`${i}-${occurrenceCount}`}
            title={title}
            className={className}
            data-toggle={dataToggle}
          >
            {accumulateRow.row.map((value, j) => (
              // eslint-disable-next-line react/no-array-index-key
              <td key={j} className={tableAligner.alignmentClass(j)}>
                {this.formatValue(value, j)}
              </td>
            ))}
          </tr>
        );
      }
      return result;
    });
    activateTooltips();
    return rows;
  };

  renderShowAll: () => null | Element<"div"> = () => {
    const { result } = this.props;
    const { loadingChunks, rowsToShowCount } = this.state;
    if (loadingChunks) {
      return null;
    } else if (
      result.row_count <= this.minRowsToShow ||
      rowsToShowCount === result.row_count
    ) {
      return (
        <div className="row-count">
          {result.row_count}
          {" rows."}
        </div>
      );
    } else {
      const rowsShown = Math.min(rowsToShowCount, result.row_count);
      return (
        <div className="row-count d-flex align-items-baseline justify-content-end">
          <span>
            Showing {rowsShown} of {result.row_count} rows.
          </span>
          <button
            className="btn btn-link btn-sm"
            type="button"
            onClick={this.handleClickMoreRows}
          >
            Show more rows
          </button>
        </div>
      );
    }
  };

  renderChartButton: () => null | Element<"button"> = () => {
    const { showChart, loadingChunks } = this.state;
    if (this.graphInfo.chartable()) {
      const chartButtonText = showChart ? "Hide chart" : "Show chart";
      return (
        <button
          type="button"
          className={this.chartButtonClass()}
          onClick={() => {
            if (!loadingChunks) {
              const shouldShowChart = !showChart;
              if (shouldShowChart) {
                this.showChart();
              } else {
                this.setState({ showChart: shouldShowChart });
              }
            }
          }}
        >
          <i className="fas fa-chart-bar"></i> {chartButtonText}
        </button>
      );
    } else {
      return null;
    }
  };

  chartButtonClass: () => string = () => {
    const { loadingChunks } = this.state;
    const baseClasses = "btn btn-outline-secondary btn-sm";
    if (loadingChunks) {
      return `${baseClasses} disabled`;
    } else {
      return baseClasses;
    }
  };

  renderAxesButton: () => null | Element<"button"> = () => {
    const { showChart, showChartConfig } = this.state;
    if (showChart) {
      const text = showChartConfig ? "Hide axes" : "Show axes";
      return (
        <button
          type="button"
          className="btn btn-outline-secondary btn-sm"
          onClick={() => this.setState({ showChartConfig: !showChartConfig })}
        >
          {text}
        </button>
      );
    } else {
      return null;
    }
  };

  renderOptionMenu: () => Element<"div"> = () => {
    const { result, debugModeEnabled } = this.props;
    return (
      <div className="d-flex justify-content-between flex-column align-items-start flex-lg-row">
        <div className="btn-group my-2">
          <ShareButton result={result} />
          <a
            className="btn btn-outline-secondary btn-sm"
            href={`/queries/${result.id}.csv`}
          >
            <i className="fas fa-file-csv"></i>{" "}
            <span className="d-none d-lg-inline">Download as </span>CSV
          </a>
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>

        <div className="btn-group my-2">
          {this.renderChartButton()}
          {this.renderAxesButton()}
        </div>
      </div>
    );
  };

  render: () => Element<"div"> = () => {
    const { result, showDataSource, onDeleteClick, updateNote } = this.props;
    const { tableAligner } = this.state;
    const dataSource = result.data_source.name;
    return (
      <div className="card border-success mb-3">
        <div className="card-header border-success bg-white">
          <ResultTime time={result.inserted_at} />
          {showDataSource && (
            <span className="small text-muted">
              {" · "}
              <a className="text-muted" href={`/data_sources/${dataSource}`}>
                {dataSource}
              </a>
            </span>
          )}
          {onDeleteClick && (
            <button
              type="button"
              className="btn btn-sm float-right"
              onClick={() => onDeleteClick(result.id)}
            >
              <i className="fas fa-times" aria-label="Delete"></i>
            </button>
          )}
          <CodeViewer statement={result.statement} />
        </div>
        <div className="card-body">
          {updateNote && !result.note && (
            <NoteButton
              initialValue={result.note}
              onChange={(newNote) => updateNote(result.id, newNote)}
            />
          )}
          <QueryNote
            id={result.id}
            note={result.note}
            updateNote={updateNote}
          />
          <InfoView info={this.getInfoMessages()} />
          <div className="result-table">
            <table className="table table-striped table-condensed table-hover">
              <thead>
                <tr>
                  {result.columns.map((column, i) => (
                    // eslint-disable-next-line react/no-array-index-key
                    <th key={i} className={tableAligner.alignmentClass(i)}>
                      {column}
                    </th>
                  ))}
                </tr>
              </thead>

              <tbody>{this.renderRows()}</tbody>
            </table>
          </div>
          {this.renderShowAll()}
          {this.renderOptionMenu()}
          {this.conditionallyRenderChartConfig()}
          {this.conditionallyRenderChart()}
        </div>
      </div>
    );
  };
}
