import React from "react";

const typeColors = {
  boolean: "#fffbc6",
  integer: "#e8ffdd",
  real: "#e7f9cf",
  text: "#fff2f2",
  date: "#e8f2ff",
  datetime: "#e9e8ff",
};

const ColumnIcon = ({ column }) => {
  if (column.key_type) {
    const icon = column.key_type === "user_id" ? "user" : "link";
    return (
      <span
        className={`type-icon ${icon}-icon`}
        data-toggle="tooltip"
        style={{
          backgroundColor: typeColors[column.type] || "transparent",
        }}
        title={`${column.key_type}:${column.type}`}
      >
        <span className={`fas fa-${icon}`}>&nbsp;</span>
      </span>
    );
  } else {
    return (
      <span
        className="type-icon native-type"
        style={{
          backgroundColor: typeColors[column.type] || "transparent",
        }}
        data-toggle="tooltip"
        title={`${column.type}`}
      >
        {column.type[0].toUpperCase()}
      </span>
    );
  }
};

export default ColumnIcon;
