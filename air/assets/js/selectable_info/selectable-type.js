export function selectableType(selectable) {
  switch (selectable.kind) {
    case "table":
      return selectable.content_type === "private"
        ? "Personal Table"
        : "Non-personal Table";
    case "analyst_table":
      return "Analyst Table";
    case "view":
      return "View";
    default:
      throw new Error(`Unknown selectable kind ${selectable.kind}`);
  }
}
