// @flow

import React from "react";

export type NumberFormat = {
  decimal_digits: number,
  decimal_sep: string,
  thousand_sep: string,
};

export const formatNumber = (value: number, format: NumberFormat): string => {
  let string = value.toLocaleString("en-US",
    {minimumFractionDigits: format.decimal_digits, maximumFractionDigits: format.decimal_digits});
  const [fixed, fractional] = string.split(".");
  string = fixed.replace(/,/g, format.thousand_sep);
  if (!Number.isInteger(value)) {
    string += format.decimal_sep + fractional;
  }
  return string;
};

export const NumberFormatExample = (props: NumberFormat) =>
  <div>
    Format example with the current settings: <b>{formatNumber(123456789.123456789, props)}</b>
  </div>;
