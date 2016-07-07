# Aggregator

This module describes algorithms used by the `Aggregator` module to preserve
anonymity.

## DISTINCT preprocessing

  Every function can be applied to a distinct version of the given column, for
  example `COUNT(DISTINCT name)` or `AVG(DISTINCT price)`. In those cases we
  apply the following preprocessing steps to a bucket of rows:

  - Split the bucket into per-value-buckets on the given column
  - Order the per-value-buckets from smallest (least users) to biggest
  - Take a single row from each per-value-bucket
  - Feed this result into the appropriate function (see docs/anonymizer.md)

  This last step ensures that the results for a query with `DISTINCT` do not
  vary greatly depending on the presence of just one user with many unique
  values in a given column, since the `Anonymizer` will discard such outliers.
  Without that an attacker would be able to deduce if certain conditions are
  met for such a victim by comparing the results of two queries. For example if
  `SELECT COUNT(DISTINCT purchased_item) FROM foo` varies greatly from `SELECT
  COUNT(DISTINCT purchased_item) FROM foo WHERE credit_card_number =
  '00000000000'` and we know a person exists who purchased a lot of items
  nobody else purchased, then we now also know that person's credit card is
  `'00000000000'`.

  A downside of this approach is that we do not report an exact result even if
  we in principle could, because each distinct value has enough users. This
  could be rectified by adding a special case to the code, but for now we
  decided that uniform processing is more important, since we don't know how
  useful that case will be.
