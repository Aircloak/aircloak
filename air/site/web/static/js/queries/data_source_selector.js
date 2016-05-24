import React from "react";

export class DataSourceSelector extends React.Component {
  constructor(props) {
    super(props);

    this.onChange = this.onChange.bind(this);
  }

  onChange(event) {
    this.props.onChange(event.target.value);
  }

  render() {
    return (
      <select onChange={this.onChange} value={this.props.selectedDataSource}>
        {this.props.sources.map((source, i) =>
          <option key={i} value={source.token}>{source.display}</option>
        )}
      </select>
    );
  }
}

DataSourceSelector.propTypes = {
  sources: React.PropTypes.arrayOf(React.PropTypes.shape({
    token: React.PropTypes.string.isRequired,
    display: React.PropTypes.string.isRequired,
  })).isRequired,
  selectedDataSource: React.PropTypes.string.isRequired,
  onChange: React.PropTypes.func,
};
