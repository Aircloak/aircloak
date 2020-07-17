import React from "react";
import type { Column } from "./columns";

const iconBackgroundColor = (column: Column): string => {
  if (column.access === "unselectable") {
    return `#e0e0e0`;
  }

  switch (column.isolated) {
    case true:
    case "failed":
    case "pending":
    case null:
      return "#ffd6d6";
    default:
      return "white";
  }
};

const iconForegroundColor = (column: Column): string => {
  return column.access === "unselectable" ? "grey" : "#212529";
};

const iconTooltip = (column: Column): string => {
  const prefix = column.key_type ? `${column.key_type}:` : "";

  if (column.access === "unselectable") {
    return `${prefix}${column.type} (unselectable)`;
  }

  switch (column.isolated) {
    case true:
    case "failed":
    case null:
      return `${prefix}${column.type} (isolating)`;
    case "pending":
      return `${prefix}${column.type} (isolator status pending)`;
    default:
      return `${prefix}${column.type}`;
  }
};

const ColumnIcon = ({ column }: { column: Column }) => {
  console.log(column);
  const style = {
    color: iconForegroundColor(column),
    backgroundColor: iconBackgroundColor(column),
  };

  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return (
      <span
        className={`type-icon ${icon}-icon`}
        style={style}
        data-toggle="tooltip"
        title={iconTooltip(column)}
      >
        <span className={`fas fa-${icon}`}>&nbsp;</span>
      </span>
    );
  } else {
    return (
      <span
        className="type-icon native-type"
        style={style}
        data-toggle="tooltip"
        title={iconTooltip(column)}
      >
        {column.type[0].toUpperCase()}
      </span>
    );
  }
};

export default ColumnIcon;
