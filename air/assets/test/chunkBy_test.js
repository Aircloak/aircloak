import assert from "assert";

import chunkBy from "audit_log/chunkBy";

describe("chunkBy", () => {
  const key = (event) => event.name;

  it("leaves different events in their own groups", () => {
    const input = [{name: "event1"}, {name: "event2"}];
    assert.deepEqual(chunkBy(input, key), [[{name: "event1"}], [{name: "event2"}]]);
  });

  it("groups consecutive events by key", () => {
    const input = [{name: "event1"}, {name: "event1"}, {name: "other event"}];
    assert.deepEqual(chunkBy(input, key), [[{name: "event1"}, {name: "event1"}], [{name: "other event"}]]);
  });

  it("does not create empty groups", () => {
    assert.deepEqual(chunkBy([], key), []);
  });

  it("splits non-consecutive groups", () => {
    const input = [{name: 1}, {name: 1}, {name: 2}, {name: 1}, {name: 1}];
    assert.deepEqual(chunkBy(input, key), [[{name: 1}, {name: 1}], [{name: 2}], [{name: 1}, {name: 1}]]);
  });

  it("keeps other data", () => {
    const input = [{name: 1, some: "data"}, {name: 1}];
    assert.deepEqual(chunkBy(input, key), [input]);
  });

  it("works for complex keys", () => {
    const input = [{a: 1, b: 2}, {a: 1, b: 2}, {a: 1, b: 3}];
    assert.deepEqual(chunkBy(input, (x) => [x.a, x.b]), [[{a: 1, b: 2}, {a: 1, b: 2}], [{a: 1, b: 3}]]);
  });
});
