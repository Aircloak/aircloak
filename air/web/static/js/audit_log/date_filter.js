import React from "react";
import DatePicker from "react-datepicker";
import moment from "moment";

export class DateFilter extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      startDate: moment().subtract(1, "week"),
      endDate: moment(),
    };

    this.handleDateStartChange = this.handleDateStartChange.bind(this);
    this.handleDateEndChange = this.handleDateEndChange.bind(this);
  }

  handleDateStartChange(startDate) {
    this.setState({startDate});
    this.props.handleDateChange(startDate, this.state.endDate);
  }

  handleDateEndChange(endDate) {
    this.setState({endDate});
    this.props.handleDateChange(this.state.startDate, endDate);
  }

  render() {
    return (
      <div className={this.props.className}>
        <span>Time interval</span>&nbsp;
        <DatePicker
          dateFormat="YYYY/MM/DD"
          showYearDropdown
          selected={this.state.startDate}
          startDate={this.state.startDate}
          endDate={this.state.endDate}
          onChange={this.handleDateStartChange}
        />
        <DatePicker
          dateFormat="YYYY/MM/DD"
          showYearDropdown
          todayButton="Today"
          selected={this.state.endDate}
          startDate={this.state.startDate}
          endDate={this.state.endDate}
          onChange={this.handleDateEndChange}
        />
      </div>
    );
  }
}

DateFilter.propTypes = {
  className: React.PropTypes.string,
  handleDateChange: React.PropTypes.func,
};
