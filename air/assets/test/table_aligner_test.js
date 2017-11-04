import assert from "assert";

import {TableAligner} from "queries/table_aligner";

describe("TableAligner", () => {
  it("returns left alignment class for text columns", () => {
    const data = new TableAligner(
      [{row: ["foo"]}],
    );
    assert.equal(data.alignmentClass(0), "text-left");
  });

  it("aligns as text if boolean", () => {
    const data = new TableAligner(
      [{row: [true]}],
    );
    assert.equal(data.alignmentClass(0), "text-left");
  });

  it("returns right alignment class for numerical columns", () => {
    const data = new TableAligner(
      [{row: [1]}],
    );
    assert.equal(data.alignmentClass(0), "text-right");
  });

  it("detects numbers even if first row is a null value", () => {
    const data = new TableAligner(
      [{row: [null]}, {row: [undefined]}, {row: [2]}],
    );
    assert.equal(data.alignmentClass(0), "text-right");
  });

  it("detects numbers even if first row is an anonymized value", () => {
    const data = new TableAligner(
      [{row: ["*"]}, {row: [2]}],
    );
    assert.equal(data.alignmentClass(0), "text-right");
  });

  it("returns left align if there are no real values", () => {
    const data = new TableAligner(
      [{row: ["*"]}, {row: [null]}, {row: [undefined]}],
    );
    assert.equal(data.alignmentClass(0), "text-left");
  });
});
