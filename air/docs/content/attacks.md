
# Attacks on Diffix Cedar

This section describes a variety of attacks against anonymization mechanisms. Some of the attacks are general in nature (can be used against multiple mechanisms). Some are specific to Diffix overall, and others specific to [Diffix Cedar](diffix.md). In what follows, the *cloak* is the device that implements Diffix.

## Table Of Contents

  - [Attack criteria](#attack-criteria)
  - [Trackers](#trackers)
  - [Attribute value inspection attacks](#attribute-value-inspection-attacks)
  - [Suppression signal attacks](#suppression-signal-attacks)
  - [Noise averaging attacks](#noise-averaging-attacks)
    - [Naive averaging](#naive-averaging)
    - [Different syntax but same semantics, with floating](#different-syntax-but-same-semantics-with-floating)
    - [Different syntax but same semantics, without floating](#different-syntax-but-same-semantics-without-floating)
    - [Different syntax and semantics, but same result](#different-syntax-and-semantics-but-same-result)
    - [Split averaging attack](#split-averaging-attack)
    - [Linear program reconstruction](#linear-program-reconstruction)
    - [JOINs with non-personal tables](#joins-with-non-personal-tables)
  - [Difference attacks](#difference-attacks)
    - [First derivative difference attack](#first-derivative-difference-attack)
    - [Difference attack with counting NULL](#difference-attack-with-counting-null)
    - [Noise exploitation attacks](#noise-exploitation-attacks)
      - [Through extreme user contribution](#through-extreme-user-contribution)
      - [Through chaff conditions](#through-chaff-conditions)
    - [Range creep with averaging](#range-creep-with-averaging)
    - [Multiple isolating negands](#multiple-isolating-negands)
  - [SQL backdoor attacks](#sql-backdoor-attacks)
  - [Side Channel attacks](#side-channel-attacks)
    - [Error generation attacks](#error-generation-attacks)
      - [Divide by zero](#divide-by-zero)
      - [Overflow](#overflow)
      - [Square root of a negative number](#square-root-of-a-negative-number)
    - [Timing attacks](#timing-attacks)
      - [JOIN timing attack](#join-timing-attack)

## Attack criteria

In the following attacks, our criteria for what determines a successful attack are the same as those used by the [EU Article 29 Data Protection Working Party Opinion 05/2014 on Anonymisation Techniques](https://cnpd.public.lu/fr/publications/groupe-art29/wp216_en.pdf), namely *singling-out*, *linkability*, and *inference*. We regard an attack as more effective when the attacker is able to make statements of the following sort with high confidence:

* *Singling-out:* "There is a single user with the following attributes."
* *Inference:* "All rows with attributes A, B, and C also have attribute D"
* *Linkability:* "The users with these attributes in the table protected by Diffix are also in a table not protected by Diffix."

A description of how these criteria are used can be found in the [Diffix Birch paper](https://arxiv.org/pdf/1806.02075.pdf)

## Trackers

A powerful class of attack, called trackers, was discovered in the late 1970s. This attack targets anonymization mechanisms where answers to queries that pertain to fewer than K individuals or more than all-but-K individuals are suppressed, but otherwise exact answers are given.

A tracker attack requires two things:
1. A pseudo-identifier: A column value or set of column values that uniquely identifies an individual.
2. A tracker: a column value that includes more than K individuals.

A pseudo-identifier can be an expression with multiple terms, like `(zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')`. The attacker does not need to know in advance that a given expression is a pseudo-identifier. This can be validated in the attack.

An example of a tracker would be `gender = 'M'` (because there are more than K males in the dataset). The victim of the attack doesn't have to be male. The tracker `gender = 'F'` would work just as well.

In the following example, the pseudo-identifier for the victim is `(zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')` and the tracker is `gender = 'M'`.

First, the attacker verifies that the pseudo-identifier indeed identifies a single individual with four queries. The first two are used to compute the total number of users in the database (here column `uid` contains a distinct value for each individual):

*Query 1*: compute *N1* as:

```
SELECT count(DISTINCT uid)
FROM database
WHERE gender = 'M'
```

*Query 2*: compute *N2* as:

```
SELECT count(DISTINCT uid)
FROM database
WHERE gender <> 'M'
```

The number of users *N* is *N = (N1 + N2)*

*Query 3*: Compute *N3*:

```
SELECT count(DISTINCT uid)
FROM database
WHERE gender = 'M' OR
      (zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')
```

*Query 4*: Compute *N4*:

```
SELECT count(DISTINCT uid)
FROM database
WHERE gender <> 'M' OR
      (zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')
```

Both counts *N3* and *N4* include all users with the pseudo-identifier. *N3* also includes all males, and *N4* includes everyone else. The sum *N3 + N4* therefore counts all users with the pseudo-identifier twice, and counts everybody else once. As such, if *(N3 + N4) - N = 1*, then this validates for the attacker that there is only one user with the pseudo-identifier.

Knowing this, the attacker can learn virtually anything about the victim. For instance, the attacker can learn the victim's salary with four queries (here assuming that each user is represented only once in the database):

*Query 1*: compute *S1* as:

```
SELECT sum(salary)
FROM database
WHERE gender = 'M'
```

*Query 2*: compute *S2* as:

```
SELECT sum(salary)
FROM database
WHERE gender <> 'M'
```

The sum of all salaries *S* is *S = (S1 + S2)*.

*Query 3*: Compute *S3*:

```
SELECT sum(salary)
FROM database
WHERE gender = 'M' OR
      (zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')
```

*Query 4*: Compute *S4*:

```
SELECT sum(salary)
FROM database
WHERE gender <> 'M' OR
      (zip = 12345 AND bday = '1975-03-11' AND univ = 'MIT')
```

Again, *S3* and *S4* include the victim, while *S3* also includes males, and *S4* also includes all other users. The salary if the victim is counted twice in *S3 + S4*, and so the victim's salary is *(S3 + S4) - S*.

Diffix defends against tracker attacks by disallowing `OR` and it's De Morgan equivalent (`NOT (A and B)` is the same as `NOT A or NOT B`). It does allow a limited form of union logic with `IN`, but this is not enough to execute a tracker attack.

`OR` can be emulated with multiple queries and corresponding arithmetic, as in `A OR B` is equivalent to `A` + `B` - `A AND B`. This would not work with the cloak both because queries with only the pseudo-identifier would be suppressed because of a low count, as well as because of the noise.

## Attribute value inspection attacks

Perhaps the most direct attack is to simply list column values. If the analyst can list a column value or set of column values that apply to a single user, then anonymity according to our goal would be violated.

A simple query for this attack would be:

```sql
SELECT ssn
FROM table
```

or for multiple columns, 

```sql
SELECT birthdate, zip, gender
FROM table
```

If the first query listed any social security numbers, then the privacy of the users with those numbers would be violated. Likewise if the second query listed users with a unique combination of birthdate, zipcode, and gender, then those users privacy would be violated according to our goal.

This attack is prevented through the use of *low-count suppression*.  Any column values that pertain to too few distinct users are suppressed by not being output.

## Suppression signal attacks

An attacker may be able to learn about individual users from whether a given bucket was suppressed or not. For instance, if a constant threshold were used for the suppress decision (i.e. suppress when 2 or fewer distinct users), then if the attacker happened to know that there is either 2 or 3 users with a certain set of attribute values, then if the bucket is not suppressed then the attacker knows that there must be three users, and potentially learns something about the third user.

To prevent this, Diffix uses a noisy threshold using sticky layered noise. Because the threshold itself can vary, in the previous example the attacker would be uncertain as to whether there were 2 or 3 users.

## Noise averaging attacks

Averaging attacks attempt to remove noise through multiple queries that somehow generate different noise samples (overcome the noise stickiness) and average away the noise.

### Naive averaging

The simplest averaging attack repeats the same query. This doesn't work because of sticky noise generates the same noise value.

### Different syntax but same semantics, with floating

Different noise samples are generated by queries that execute identical query semantics but with different syntax in the hope that the cloak does not recognize the same syntax and so produces a different noise sample.

A simple example would be two queries with the following `WHERE` clauses:

```
Q1: WHERE age = 10
Q2: WHERE age + 1 = 11
```

In principle, the cloak could interpret the math and determine that the above two expressions are identical. This can get extremely difficult, however, as the math gets more complex. Therefore for any expression that uses math, the cloak *floats* the column value by re-writing the SQL so that the column in question (here `age`) is brought up to the outer `SELECT` so that the actual selected values can be examined. 

However, other examples exist:

```
Q1: WHERE left(column,5)
Q2: WHERE substring(column FROM 1 FOR 5)
```

Not so many different semantically identical conditions can be made with string manipulation as with math manipulation, but nevertheless the above conditions would be floated to ensure that the seeds are identical.

### Different syntax but same semantics, without floating

The cloak doesn't always float. It does not float `col BETWEEN val1 AND val2` for instance. Rather, it deduces the seed material from SQL inspection. There are cases where same semantic but different syntax conditions can be generated from non-floated conditions. In a few cases, the cloak does not correctly account for the different syntax and uses different seeding material.

One such case is `BETWEEN` versus `bucket()`. For instance the range `100-200` can be generated with both of the following queries:

```
Q1: SELECT bucket(column BY 100) ....
Q2: WHERE column BETWEEN 100 and 200
```
The cloak, however, does not detect that these semantics are identical and generates different seeds as a result. Through this trick the analyst can get two noise samples for the layers associated with these conditions, and therefore reduce the overall noise somewhat.

In many other cases, however, the cloak does seed properly.

Text datatypes for instance are all converted to lower case for the purpose of seeding, so that the `lower()` and `upper()` functions can't be used to derive additional noise samples. 

### Different syntax and semantics, but same result

In Diffix Cedar, an analyst can obtain multiple samples for `<>` text conditions using `substring()`. For instance, suppose that a text column contains two values, 'ABCDE' and 'FGHIJ'. Each of the following conditions would generate a different seed even though the underlying answers are the same:

```
WHERE substring(col for 1) <> 'A'
WHERE substring(col for 2) <> 'AB'
WHERE substring(col for 2) <> 'ABC'
...
```

While this is certainly unfortunate, there are no known attacks that can successfully exploit this weakness.

### Split averaging attack

This attack creates pairs of queries where the sum of the queries in each pair have the same underlying value (i.e. before noise), but where each pair uses semantically different conditions from all other pairs, thus producing different noise values that can be averaged. This can be done with `WHERE` clauses of the following sort:

```
Pair 1:
Q1: WHERE column = 'X'
Q2: WHERE column <> 'X'
Pair 2:
Q1: WHERE column = 'Y'
Q2: WHERE column <> 'Y'
```

From pair 1, the first query Q1 matches all rows where `column = 'X'`, and Q2 matches all other columns (not equal to 'X'). The sum of Q1 and Q1 therefore includes all rows (that match other conditions, not shown here). The same holds for the two queries from pair 2. However, all of these conditions are semantically different and so produce different noise samples.

The cloak defends against this through *layered noise*. Each condition generates its own noise layers, which are summed together to produce the final noise value. The layers from the above `WHERE` clauses would indeed be averaged away, but the noise from other conditions in the queries would not average out. As a simple example, the following two queries are from one pair where the attacker goal is to learn the exact number of women:

Query 1:
```sql
SELECT count(DISTINCT uid)
FROM table
WHERE age = 20 and gender = 'F'
```

Query 2:
```sql
SELECT count(DISTINCT uid)
FROM table
WHERE age <> 20 and gender = 'F'
```

While the noise layers for `age` would indeed average out, the static noise layer for `gender = 'F'` would be the same across all queries in the attack and would result in a noisy final count.

### Linear program reconstruction

In this attack, the analyst generates queries where each query selects a different set of users, but where any given user is selected by a substantial number of queries. Repeatedly selecting among a pseudo-random subset of users would have this effect. The analyst then generates a set of equations and solves for the presence or absence of each user in that set of equations.

This attack was successfully run against Diffix Birch using the following WHERE clause:

```
WHERE floor(5 * pow((client_id * 2),0.5) + 0.5) = floor(5 * pow((client_id * 2), 0.5))
```

where different constants were used to effectly select different users.

The current defense is to force `clear` conditions (no math) on columns that have a substantial fraction of user-unique values.

### JOINs with non-personal tables

The cloak does not anonymize non-personal tables. Therefore, analysts are allowed to see the complete tables. The cloak allows JOIN, but with strict limitations. Without these limitations, an analyst could potentially exploit knowledge of non-personal tables to generate multiple samples and average away noise.

A typical non-personal table might contain a numeric identifier and a text field. For instance, a `product_name` table might have columns `product_id` and `name`. This table is non-personal because it contains no user information. It may be linked to for instance a `purchases` table with the `product_id` as key.

Suppose an analyst wished to remove noise associated with the condition `age = 20`, and there are multiple non-personal tables of various sorts. The analyst could then build a set of queries with different `JOIN` conditions as follows:

```
JOIN (...) ON users.age = product_name.id - 28788 WHERE product_name.value = 'Juice Squeezer'
JOIN (...) ON users.age = error_codes.id - 232 WHERE error_codes.value = 'Bad Input'
JOIN (...) ON users.age = cc_types.id + 16 WHERE cc_types.value = 'VISA'
etc.
```

The ID of the non-personal table, after arithmetic, matches the target age (20). Because the table name is included in the seed material for the condition, each of the noise layers associated with the `ON` condition could be averaged away.

The fix is to limit `ON` conditions to simple `col1 = col2` expressions, and limit which columns can be keys through configuration. With these limitations, any meaningful attack is so unlikely that no noise layers for `ON` conditions are necessary.

## Difference attacks

In difference attacks, the attacker creates pairs of queries where the underlying true answers are either identical or differ by one user. The attacker then tries to determine which is the case.

A simple example would be the following pair of queries:

Query 1:
```sql
SELECT count(*)
FROM table
WHERE salary BETWEEN 100000 AND 110000 and
      ssn <> '539-54-9355'
```

Query 2:
```sql
SELECT count(*)
FROM table
WHERE salary BETWEEN 100000 AND 110000
```

The victim in this attack is the user with social security number (ssn) '539-54-9355'. The unknown attribute (the thing the attacker wants to learn) is whether or not the salary is in the range 100000 to 110000. Query 1 definitely excludes the victim, while Query 2 includes the victim if the victim has that salary range.

The simplest approach for the attacker is to deduce that the victim is in query 2 so long as `count(*)` is greater in query 2. Because of the noise, however, such an approach would often produce the wrong deduction.

The greater the difference between the two queries, however, the more likely the victim is indeed in that salary range. When the difference is large, the probability that the difference is due purely to the Gaussian distribution of noise is much less than the probability that the difference is due to both the noise distribution and the difference in the underlying true answer.

It is relatively rare, however, that the noise value is large enough to give the attacker a high-confidence deduction (like one in 10000 attacks).

### First derivative difference attack

There is a form of difference attack whereby the analyst generates a histogram of bucket pairs under the condition that the victim is not in the first query of each pair, and is in one and only one bucket of the second query. An example is the following:

Query 1:
```sql
SELECT bucket(salary BY 10000), count(*)
FROM table
WHERE ssn <> '539-54-9355'
```

Query 2:
```sql
SELECT bucket(salary BY 10000), count(*)
FROM table
```

If the noise layers were only static (based purely on the conditions themselves), then the difference in the noise between each pair of buckets would be the same except for the one containing the victim. This difference in the difference would identify the victim's salary.

The uid-based noise layers, however, add additional noise that is different between every pair of buckets.

### Difference attack with counting NULL

The effect of inserting safe math functions is that column values may be NULL. This gives an attacker the opportunity to force a single user to have the only NULL value in the column. The attacker can determine if the NULL value is present by comparing the outputs of `count(*)` and `count(col)`. The former counts the NULL rows, the latter does not.

For example, the following difference attack can be used to learn the value of the `cli_district_id` of the user that has a transaction amount of 24615.

```
select cli_district_id, count(foo)
from (
	select uid, cli_district_id, 1/(amount - 24615) AS foo
	from transactions ) t
group by 1
```

```
select cli_district_id, count(*)
from (
	select uid, cli_district_id, 1/(amount - 24615) AS foo
	from transactions ) t
group by 1
```

The cloak defends against this by adding an additional UID noise layer for `count(col)`.

### Noise exploitation attacks

#### Through extreme user contribution

The magnitude of the noise must be in some sense proportional to the amount that the most extreme users contribute to an answer. For instance, suppose that one knows that roughly the average salary in a database excluding the CEO. By querying for the average salary, the amount by which it differs from the known average reveals the CEO's salary. The noise needs to be large enough to mask this salary. This is called *proportional noise*.

However, with a difference attack, the amount of noise itself can reveal the presence or absence of the CEO in a query. If the CEO has a substantially higher salary than anyone else, and if the query that may include the CEO has a lot of noise, then one can conclude that the CEO is present in the query.

To prevent this, the cloak removes a small number of the most extreme values in a given query (based on a noisy threshold), and then bases the amount of noise on the average value of a small group of users with the next most extreme values (also based on a noisy threshold). This is called *flattening*.

#### Through chaff conditions

In this difference attack, the attacker exploits the uid-based noise by intentionally adding conditions that have no impact on the true underlying answer, but increase the cumulative amount of noise. The bucket pair that differs the most is most likely the one containing the user.

A simple way to do this is to add conditions like `age <> 1000`, `age <> 1001` and so on. These are called chaff conditions. In pairs where the underlying set of users is the same, the uid-based noise values are the same for each query of the pair. In pairs where the underlying set of users is different, the uid-based noise value is different.

Alternatively, a range condition such as `age BETWEEN 0 and 1000` can also be a chaff condition. Likewise the implicit ranges `round()` and `trunc()` can be chaff conditions. This is because these functions can span the entire number range of a column with one bucket.

The chaff conditions can either all be added to the same query, or added one at a time to multiple pairs, and then summing the results across the first queries of each pair and separately the second queries.

### Range creep with averaging

One way to do a difference attack is to grow a range (`BETWEEN`) so that one additional user is included in the modified range. The noise layers associated with the range defeat a simple version of this attack that uses two queries. However, if an attacker could incrementally modify a range so that each change does not change the underlying answer, then the different noise values could be averaged. If the attacker averaged away the noise in this fashion for both the smaller and larger ranges, then the noise could no longer defend against the attack.

To defend against this, the cloak forces ranges to fall within preset range offsets and sizes. This makes it very unlikely that an attacker could get enough samples both excluding and including the victim.

### Multiple isolating negands

If an analyst could make multiple different negative anded conditiond (negands) isolating the same individual user, then they could be averaged out in the context of a difference attack. For instance, `account_number <> 12345`, `social_security_number <> 123-45-6789`, `login_name <> 'alex433`, `email_address <> 'alex433@gmail.com` are all values that can isolate a single user. If each were used as the single negand, then the negands could be averaged away.

This negands are rejected by virtue of not being represented in the shadow table.

## SQL backdoor attacks

A backdoor attack is where the attacker avoids defense mechanisms by encoding conditions indirectly through math. For instance, the following query:

```sql
SELECT count(*)
FROM table
WHERE age = 30 OR age = 40
```

Can be encoded in the following query without using a `WHERE` clause:

```sql
SELECT count(*), age 30 or 40 FROM (
    SELECT uid, (age 30 + age 40) % 2 as age 30 or 40
    FROM (
        SELECT uid,
            floor((age greater 29 + age less 31) / 2) AS age 30,
            floor((age greater 39 + age less 41) / 2) AS age 40
        FROM (
            SELECT uid,
                ceil((age - 29) / 100) AS age greater 29,
                ceil(0 - (age - 31) / 100) AS age less 31,
                ceil((age - 39) / 100) AS age greater 39,
                ceil(0 - (age - 41) / 100) AS age less 41
            FROM table
        ) x
    ) y
) z
GROUP BY age 30 or 40;
```

To prevent this, the cloak limits the amount of math, particularly non-continuous functions like `ceil` and `floor`, that can appear in a query.

## Side Channel attacks

### Error generation attacks

#### Divide by zero

Say that the victim's birthdate is known to be 14/12/1957, and zipcode is 60036.
The following query would trigger a divide-by-zero if the victim has a salary of 100000.

```sql
SELECT count(*) FROM
  (SELECT uid, z/(6003614.195712-z+d+y+m) FROM
     (SELECT uid, zipcode * 100 AS z,
             year(bday) / 10000 AS y,
             month(bday) / 1000000 AS m,
             day(bday) as d
      FROM user_info
      WHERE salary = 100000
     ) t1
  ) t2
```

Some databases throw and exception when divide-by-zero occurs. In these cases, the exception itself signals the salary of the victim.

#### Overflow

In some database, a numeric overflow throws an exception. This can be exploited, at least in Postgres, with for instance with the following attack.

```
select count(*)
from accounts
where lastname = 'Zamora' and
      birthdate = '1996-11-17' and
      2^(10000.01 * cli_district_id) = 123.12
```

The third condition in the `WHERE` clause causes an overflow if it is executed.  Assuming that there is only a single user with the lastname 'Zamora', if Zamora does not have the indicated birthdate, then the third condition won't be executed and a suppressed output is given. If Zamora does have that birthdate, then the third condition throws an exception. The exception is transmitted to the analyst as an execution error.

The cloak defends against this by installing and executing "safe" math routines in the database. The safe routines capture exceptions and returns NULL rather than throwing an exception. As a result there is no error signal transmitted to the analyst, and therefore the analyst doesn't know if an exception took place or not.

Unfortunately the safe math routines slow down query execution. To minimize this performance hit, the cloak also makes a conservative estimate as to whether or not a given math expression *might* result in an exception. If not, then the safe math routine is not executed.


#### Square root of a negative number

### Timing attacks

#### JOIN timing attack

Because of optimizations in some database implementations, a query such as the following can be used in a timing attack:

```sql
SELECT count(*)
FROM (
  SELECT uid
  FROM accounts
  WHERE lastname = 'Zamora' AND birthdate = '1996-11-17' AND
        salary = 100000
) t1 JOIN (
  SELECT distinct uid
  FROM transactions
) t2 on t1.uid = t2.uid
```

The optimization here is that, if the left `JOIN` expression (`t1`) returns zero rows, then the right `JOIN` expression is either not executed or terminated before completion. Otherwise the right `JOIN` expression is executed to completion.

If the right `JOIN` expression takes a long time to compute, as this one does, then the analyst can determine whether or not the left `JOIN` expression has zero rows or not.

In the example above, the analyst may have know that there is only one user with name 'Zamora' and birthdate '1996-11-17'. If Zamora also has this salary, then the query execution time is shorter. If not, then the query execution time is longer.
[ghi3691](https://github.com/Aircloak/aircloak/issues/3691)
