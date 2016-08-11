import React from "react";

export class AuditLogEntryView extends React.Component {
  constructor(props) {
    super(props);

    this.shouldBeVisible = this.shouldBeVisible.bind(this);
    this.containsFilterText = this.containsFilterText.bind(this);
  }

  shouldBeVisible() {
    const requiredFiltersCount = this.props.filters.length;
    let satisfiedFiltersCount = 0;

    for (const filter of this.props.filters) {
      if (this.containsFilterText(filter, this.props.entry.user) ||
          this.containsFilterText(filter, this.props.entry.event) ||
          this.containsFilterText(filter, this.props.entry.inserted_at)) {
        satisfiedFiltersCount = satisfiedFiltersCount + 1;
      } else {
        let satisfied = false;
        for (const data of this.props.entry.metadata) {
          if (this.containsFilterText(filter, data[0]) ||
              this.containsFilterText(filter, data[1])) {
            satisfied = true;
          }
        }
        if (satisfied) {
          satisfiedFiltersCount = satisfiedFiltersCount + 1;
        }
      }
    }
    return requiredFiltersCount === satisfiedFiltersCount;
  }

  containsFilterText(filter, text) {
    return (text.toLowerCase().indexOf(filter) !== -1);
  }

  render() {
    if (! this.shouldBeVisible()) {
      return null;
    }

    const metadata = this.props.entry.metadata.map((keyval, i) =>
      <li key={i}><strong>{keyval[0]}</strong>: {keyval[1]}</li>
    );
    return (
      <tr>
        <td>{this.props.entry.event}</td>
        <td>{this.props.entry.user}</td>
        <td>{this.props.entry.inserted_at}</td>
        <td><ul>{metadata}</ul></td>
      </tr>
    );
  }
}

AuditLogEntryView.propTypes = {
  entry: React.PropTypes.shape({
    user: React.PropTypes.string.isRequired,
    event: React.PropTypes.string.isRequired,
    inserted_at: React.PropTypes.string.isRequired,
    metadata: React.PropTypes.arrayOf(React.PropTypes.array),
  }),
  filters: React.PropTypes.arrayOf(React.PropTypes.string),
};
