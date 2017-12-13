defmodule Cloak.Regressions.TeamBank.Test do
  use ExUnit.Case, async: true

  test "simple query 1" do
    query = """
    SELECT
      bankzugang.bankname,
      bankzugang.blz,
      count(bankzugang.inhaberId),
      count(distinct bankzugang.inhaberId),
      count(*)
    FROM (#{vb_view()}) as VB JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId
    Group By 1,2
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "simple query 2" do
    query = """
    SELECT
      count(bankzugang.inhaberId),
      count(distinct bankzugang.inhaberId),
      count(*)
    FROM (#{vb_view()}) as VB JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId AND bankzugang.blz NOT IN ('90090042','76032000')
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "simple query 3" do
    query = """
    SELECT
      count(bankzugang.inhaberId),
      count(distinct bankzugang.inhaberId),
      count(*)
    FROM (#{vb_view()}) as VB JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "simple query 4" do
    query = """
    SELECT bankzugang.bankname, count(bankzugang.inhaberId), count(distinct bankzugang.inhaberId), count(*)
    FROM (#{vb_view()}) as VB JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId Group By 1
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "simple query 5" do
    query = """
    SELECT bankzugang.blz, count(bankzugang.inhaberId), count(distinct bankzugang.inhaberId), count(*)
    FROM (#{vb_view()}) as VB JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId Group By 1
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 1" do
    query = """
    SELECT
      bankzugang.bankname,
      bankzugang.blz,
      count(distinct bankzugang._id) AS num_bankzugang,
      count(distinct kontoId) AS num_konto,
      count(distinct Acc.kontoinhaber) AS num_Acc
    FROM
      (SELECT
        konto.inhaberId AS kontoinhaber,
        konto._id AS kontoId,
        bankzugang.blz,
        bankzugang.bankname,
        bankzugang._id AS bankzugangId,
        bankzugang.inhaberId AS bankzuganginhaber
      FROM bankzugang JOIN konto ON konto.bankzugangId = bankzugang._Id AND konto.inhaberId = bankzugang.inhaberId
      WHERE blz NOT IN ('47110815', '90090042')
    ) AS Acc
      JOIN bankzugang ON bankzugang.inhaberId = Acc.bankzuganginhaber AND Acc.kontoinhaber = bankzugang.inhaberId
    WHERE bankzugang._id <> Acc.bankzugangId AND bankzugang.blz NOT IN ('47110815', '90090042')
    GROUP BY 1,2
    ORDER BY 3 DESC
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 2" do
    query = """
    SELECT
      bankzugang.bankname,
      bankzugang.blz,
      count(bankzugang.inhaberId) AS num_bankzugang,
      count_noise(bankzugang.inhaberId),
      count(distinct bankzugang.inhaberId) AS num_Acc,
      count_noise(distinct bankzugang.inhaberId)
    FROM (#{vb_view()}) as VB
      JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subbankzugangId AND
      bankzugang.blz NOT IN ('90090042','76032000')
    Group By 1,2
    Order By num_bankzugang DESC;
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 3" do
    query = """
    SELECT
      VB.bankname,
      VB.blz,
      count(VB.VB_USER_KONTO) AS Anzahl_Konten,
      count_noise(VB.VB_USER_KONTO),
      count(distinct VB.VB_USER_KONTO) AS Anzahl_Acc,
      count_noise(distinct VB.VB_USER_KONTO)
    FROM (#{vb_view()}) as VB
      JOIN bankzugang on bankzugang.inhaberId = VB.VB_USER_KONTO
    WHERE bankzugang._id <> VB.subBankzugangId
    Group By VB.blz, VB.bankname
    Order By Anzahl_Konten DESC;
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 4" do
    query = """
    Select
      bucket(kontostand.betrag BY 1000 ALIGN MIDDLE) AS kontostand,
      count(distinct konto.inhaberId) AS num_Acc, count_noise(distinct konto.inhaberId)
    FROM bankzugang
      JOIN konto ON konto.inhaberId = bankzugang.inhaberId AND
      blz NOT IN ('90090042', '47110815') AND konto.bankzugangId = bankzugang._id
    JOIN umsatz ON umsatz.inhaberId = bankzugang.inhaberId AND umsatz.kontoId = konto._id
    WHERE
      UPPER(umsatzeigenschaften.spezifizierung) IN ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)',
        'BEZÜGE', 'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE')
      AND letzterUmsatz.buchungsDatum >= '2017-10-29' AND letzterUmsatz.buchungsDatum < '2017-11-28'
    GROUP BY kontostand
    ORDER BY kontostand
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 5" do
    query = """
    Select
      bucket(betrag by 500 ALIGN MIDDLE) AS income_class,
      bucket(kontostand.betrag BY 500 ALIGN MIDDLE) AS kontostand,
      count(distinct konto.inhaberId) AS num_Acc, count_noise(distinct konto.inhaberId)
    FROM bankzugang
      JOIN konto ON konto.inhaberId = bankzugang.inhaberId AND
      blz NOT IN ('90090042', '47110815') AND konto.bankzugangId = bankzugang._id
    JOIN umsatz ON umsatz.inhaberId = bankzugang.inhaberId AND umsatz.kontoId = konto._id
    WHERE
      UPPER(umsatzeigenschaften.spezifizierung) IN ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)',
        'BEZÜGE', 'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE')
      AND letzterUmsatz.buchungsDatum >= '2017-10-29' AND letzterUmsatz.buchungsDatum < '2017-11-28'
    GROUP BY income_class, kontostand
    ORDER BY income_class, kontostand
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 6" do
    query = """
    Select
      UPPER(umsatzeigenschaften.anbieter) AS Anbieter,
      count(distinct umsatz.inhaberId), count_noise(distinct umsatz.inhaberId)
    FROM(
      SELECT b._id, b.inhaberId
      FROM konto JOIN bankzugang AS b ON konto.bankzugangId = b._id AND konto.inhaberId = b.inhaberId
      WHERE
      --- hier die individuelle Bank; sonst im Select nach bankname, blz + group by
      blz = '10090000'
    ) AS Bank
      JOIN konto ON konto.bankzugangId = Bank._id AND konto.inhaberId = Bank.inhaberId
      JOIN umsatz ON umsatz.kontoId = konto._id AND umsatz.inhaberId = konto.inhaberId
      JOIN bankzugang ON bankzugang._id = konto.bankzugangId AND bankzugang.inhaberId = konto.inhaberId
    WHERE blz NOT IN ('47110815', '90090042')
      AND UPPER(umsatzeigenschaften.unterKategorie) = 'KREDITE'
      AND buchungsDatum BETWEEN '2017-07-01' AND '2018-01-01'
    GROUP BY 1
    ORDER BY 2 DESC
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "bianca 7" do
    query = """
    Select
      bankverbindung.bankname ,
      SUBSTRING(bankverbindung.iban FROM 5 FOR 8),
      count(inhaberId) AS num_Kredite,
      count(distinct inhaberId) AS num_Acc_mitKredit,
      count_noise(distinct inhaberId), avg(betrag)
    FROM umsatz
    WHERE
      UPPER(umsatzeigenschaften.unterKategorie) = 'KREDITE' AND
      UPPER(umsatzeigenschaften.spezifizierung) IN ('EASYCREDIT','KREDIT','STUDIENKREDIT','BILDUNGSKREDIT') AND
      umsatz.betrag >= -1000000 AND umsatz.betrag <0 AND
      bankverbindung.iban IS NOT NULL
    GROUP BY 1,2
    ORDER BY 1,2
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2188)
  # test "sebastian 1" do
  #   query = """
  #   SELECT
  #     bucket(avg_betrag by 500 align middle) AS income_class,
  #     bucket(avg_kontostand by 500 align middle) AS kontostand,
  #     count(distinct inhaberId) AS num_Acc,
  #     count_noise(distinct inhaberId)
  #   FROM (
  #     SELECT
  #       konto.inhaberId,
  #       avg(betrag) as avg_betrag,
  #       avg(kontostand.betrag) as avg_kontostand
  #     FROM konto INNER JOIN bankzugang
  #       ON konto.inhaberId = bankzugang.inhaberId AND
  #         blz NOT IN ('90090042', '47110815') AND
  #         konto.bankzugangId = bankzugang._id
  #     INNER JOIN (
  #       SELECT
  #         inhaberId,
  #         kontoId,
  #         betrag
  #       FROM umsatz
  #       WHERE
  #         UPPER(umsatzeigenschaften.spezifizierung) IN
  #           ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)', 'BEZÜGE',
  #           'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE') AND
  #         buchungsDatum BETWEEN '2017-10-29' AND '2017-11-28'
  #     ) as umsatz ON umsatz.inhaberId = konto.inhaberId AND
  #       umsatz.kontoId = konto._id
  #     WHERE konto.letzterUmsatz.buchungsDatum BETWEEN '2017-10-29' AND '2017-11-28'
  #     GROUP BY konto.inhaberId
  #   ) average_values
  #   GROUP BY income_class, kontostand
  #   ORDER BY income_class, kontostand
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 2" do
    query = """
    SELECT
      bucket(kontostand.betrag BY 1000 ALIGN MIDDLE) AS kontostand,
      count(distinct konto.inhaberId) AS num_Acc,
      count_noise(distinct konto.inhaberId)
    FROM bankzugang
      JOIN konto ON konto.inhaberId = bankzugang.inhaberId
        AND blz NOT IN ('90090042', '47110815') AND konto.bankzugangId = bankzugang._id
      JOIN umsatz ON umsatz.inhaberId = bankzugang.inhaberId AND umsatz.kontoId = konto._id
    WHERE
      UPPER(umsatzeigenschaften.spezifizierung) IN ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)',
        'BEZÜGE', 'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE')
      AND letzterUmsatz.buchungsDatum >= '2017-10-29' AND letzterUmsatz.buchungsDatum < '2017-11-28'
    GROUP BY kontostand
    ORDER BY kontostand
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 3" do
    query = """
    SELECT
      bucket(betrag by 500 ALIGN MIDDLE) AS income_class,
      bucket(kontostand.betrag BY 500 ALIGN MIDDLE) AS kontostand,
      count(distinct konto.inhaberId) AS num_Acc,
      count_noise(distinct konto.inhaberId)
    FROM bankzugang
      JOIN konto ON konto.inhaberId = bankzugang.inhaberId
        AND blz NOT IN ('90090042', '47110815') AND konto.bankzugangId = bankzugang._id
      JOIN umsatz ON umsatz.inhaberId = bankzugang.inhaberId AND umsatz.kontoId = konto._id
    WHERE
      UPPER(umsatzeigenschaften.spezifizierung) IN ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)',
        'BEZÜGE', 'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE')
      AND letzterUmsatz.buchungsDatum >= '2017-10-29' AND letzterUmsatz.buchungsDatum < '2017-11-28'
    GROUP BY income_class, kontostand
    ORDER BY income_class, kontostand
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 4" do
    query = """
    SELECT
      bucket(kontostand.betrag BY 500 ALIGN MIDDLE) AS kontostand,
      count(distinct konto.inhaberId) AS num_Acc,
      count_noise(distinct konto.inhaberId)
    FROM bankzugang
      JOIN konto ON konto.inhaberId = bankzugang.inhaberId AND blz NOT IN ('90090042', '47110815')
        AND konto.bankzugangId = bankzugang._id
      JOIN umsatz ON umsatz.inhaberId = bankzugang.inhaberId AND umsatz.kontoId = konto._id
    WHERE
      UPPER(umsatzeigenschaften.spezifizierung) IN ('LOHN', 'GEHALT', 'LOHN/GEHALT', 'BEZÜGE (BEAMTE)',
        'BEZÜGE', 'LOHN_GEHALT', 'BEZUEGE_BEAMTE', 'BEZUEGE')
      AND letzterUmsatz.buchungsDatum >= '2017-10-29' AND letzterUmsatz.buchungsDatum < '2017-11-28'
    GROUP BY kontostand
    ORDER BY kontostand
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 5" do
    query = """
    SELECT bankname, bank_location, count(*) FROM (
      SELECT
        inhaberId,
        bankname,
        -- First 3 digits in BLZ in show region of bank
        left(cast(blz as text), 3) as bank_location
      FROM bankzugang
    ) bank
    GROUP BY bankname, bank_location
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2188)
  # test "sebastian 6" do
  #   query = """
  #   SELECT
  #     bucket(income by 200 align middle) as income_class,
  #     count(*),
  #     count_noise(*)
  #   FROM (
  #     SELECT
  #       income_by_month.inhaberId,
  #       -- Use median to not have outlier months impact values
  #       median(monthly_income) as income
  #     FROM (
  #       SELECT
  #         inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         sum(betrag) as monthly_income
  #       FROM umsatz
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY inhaberId, year_month
  #     ) as income_by_month
  #     GROUP BY income_by_month.inhaberId
  #   ) customer_incomes
  #   GROUP BY income_class
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  # Related issue: (#2188)
  # test "sebastian 7" do
  #   query = """
  #   SELECT
  #     bucket(number_of_transactions by 10 align middle) as num_transactions,
  #     count(*),
  #     count_noise(*)
  #   FROM (
  #     SELECT
  #       inhaberId,
  #       median(transactions_count) as number_of_transactions
  #     FROM (
  #       SELECT
  #         inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         count(*) as transactions_count
  #       FROM umsatz
  #       GROUP BY inhaberId, year_month
  #     ) as transactions_per_month
  #     GROUP BY inhaberId
  #   ) as num_monthly_transactions
  #   GROUP BY num_transactions
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 8" do
    query = """
    SELECT
      EXTRACT_WORDS(
        buchungstext || verwendungszweck || name
      ) as shop,
      count(*)
    FROM umsatz
    WHERE EXTRACT_WORDS(
      buchungstext || verwendungszweck || name
    ) IS NOT NULL
    GROUP BY shop
    ORDER BY count(*) DESC
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2208)
  # test "sebastian 9" do
  #   query = """
  #   SELECT
  #     year(buchungsDatum) as year,
  #     month(buchungsDatum) as month,
  #     EXTRACT_WORDS(
  #       UPPER(buchungstext || verwendungszweck || name)
  #     ) as card,
  #     count(*),
  #     count_noise(*)
  #   FROM umsatz
  #   WHERE EXTRACT_WORDS(
  #     UPPER(buchungstext || verwendungszweck || name)
  #   ) IN ('EC', 'Visa', 'Master', 'Maestro', 'American')
  #   GROUP BY card, year, month
  #   ORDER BY year, month, count(*) DESC
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 10" do
    query = """
    SELECT
      name,
      avg(kontostand.betrag),
      avg_noise(kontostand.betrag),
      max(kontostand.betrag),
      min(kontostand.betrag)
    FROM konto
    WHERE name IS NOT NULL
    GROUP BY name
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2188)
  # test "sebastian 11" do
  #   query = """
  #   SELECT
  #     name,
  #     bucket(customer_incomes.income by 200) as income_class,
  #     avg(kontostand.betrag),
  #     avg_noise(kontostand.betrag),
  #     max(kontostand.betrag),
  #     min(kontostand.betrag)
  #   FROM (
  #     SELECT
  #       income_by_month.inhaberId,
  #       -- Use median to not have outlier months impact values
  #       median(monthly_income) as income
  #     FROM (
  #       SELECT
  #         inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         sum(betrag) as monthly_income
  #       FROM umsatz
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY inhaberId, year_month
  #     ) as income_by_month
  #     GROUP BY income_by_month.inhaberId
  #   ) customer_incomes INNER JOIN konto ON
  #     customer_incomes.inhaberId = konto.inhaberId
  #   WHERE name IS NOT NULL
  #   GROUP BY name, income_class
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  # Related issue: (#2188)
  # test "sebastian 12" do
  #   query = """
  #   SELECT users.uid
  #   FROM (
  #     SELECT inhaberId as uid, sum(betrag) as monthly_income
  #     FROM umsatz
  #     WHERE betrag >= 0 and betrag < 100000
  #     GROUP BY uid
  #   ) as users
  #   WHERE users.monthly_income >= 5000 and users.monthly_income < 10000
  #   GROUP BY 1
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 13" do
    query = """
    SELECT
      left(cast(buchungsDatum as text), 7) as month,
      extract_words(buchungstext) as card,
      count(*)
    FROM umsatz
    GROUP BY month, card
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2188)
  # test "sebastian 14" do
  #   query = """
  #   SELECT
  #     bucket(income by 200 align middle) as income_class,
  #     count(*)
  #   FROM (
  #     SELECT inhaberId, median(monthly_income) as income FROM (
  #       SELECT
  #         inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         sum(betrag) as monthly_income
  #       FROM umsatz
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY inhaberId, year_month
  #     ) as income_by_month
  #     GROUP BY inhaberId
  #   ) as user_incomes
  #   GROUP BY income_class
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  # Related issue: (#2188)
  # test "sebastian 15" do
  #   query = """
  #   SELECT
  #     institution,
  #     bucket(income by 200 align middle) as income_class,
  #     count(*)
  #   FROM (
  #     SELECT inhaberId, institution, median(monthly_income) as income FROM (
  #       SELECT
  #         umsatz.inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         left(bic, 4) as institution,
  #         sum(betrag) as monthly_income
  #       FROM umsatz INNER JOIN bankzugang
  #         ON umsatz.inhaberId = bankzugang.inhaberId
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY umsatz.inhaberId, year_month, institution
  #     ) as income_by_month
  #     GROUP BY inhaberId, institution
  #   ) as user_incomes
  #   GROUP BY institution, income_class
  #   ORDER BY institution, income_class
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 16" do
    query = """
    SELECT
      left(cast(buchungsDatum as text), 7) as month,
      extract_words(verwendungszweck) as shop,
      avg(betrag),
      min(betrag),
      max(betrag),
      count(*)
    FROM umsatz
    GROUP BY month, shop
    ORDER BY month desc, shop asc
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2188)
  # test "sebastian 17" do
  #   query = """
  #   SELECT
  #     extract_words(verwendungszweck) as shop,
  #     bucket(income by 200 align middle) as income_class,
  #     count(*)
  #   FROM (
  #     SELECT inhaberId, median(monthly_income) as income
  #     FROM (
  #       SELECT
  #         inhaberId,
  #         left(cast(buchungsDatum as text), 7) as year_month,
  #         sum(betrag) as monthly_income
  #       FROM umsatz
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY inhaberId, year_month
  #     ) as income_by_month
  #     GROUP BY inhaberId
  #   ) customer_incomes INNER JOIN umsatz ON
  #     umsatz.inhaberId = customer_incomes.inhaberId
  #   GROUP BY income_class, shop
  #   ORDER BY shop ASC, income_class ASC
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  # Related issue: (#2188)
  # test "sebastian 18" do
  #   query = """
  #   SELECT
  #     bucket(income by 200 align middle) as income_class,
  #     avg(monthly_savings)
  #   FROM (
  #     SELECT
  #       expenses_by_month.inhaberId,
  #       -- Use median to not have outlier months impact values
  #       median(monthly_income) as income,
  #       median(monthly_income) + median(monthly_expenses) as monthly_savings
  #     FROM (
  #       SELECT
  #         inhaberId,
  #         month(buchungsDatum) as month,
  #         year(buchungsDatum) as year,
  #         sum(betrag) as monthly_income
  #       FROM umsatz
  #       WHERE betrag >= 0 and betrag < 1000000000
  #       GROUP BY inhaberId, month, year
  #     ) as income_by_month INNER JOIN (
  #       SELECT
  #         inhaberId,
  #         month(buchungsDatum) as month,
  #         year(buchungsDatum) as year,
  #         sum(betrag) as monthly_expenses
  #       FROM umsatz
  #       WHERE betrag >= -1000000000 and betrag < 0
  #       GROUP BY inhaberId, month, year
  #     ) as expenses_by_month ON
  #       income_by_month.inhaberId = expenses_by_month.inhaberId and
  #       income_by_month.year = expenses_by_month.year and
  #       income_by_month.month = expenses_by_month.month
  #     GROUP BY expenses_by_month.inhaberId
  #   ) customer_incomes
  #   GROUP BY income_class
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  test "sebastian 19" do
    query = """
    SELECT
      expenses_by_month.inhaberId,
      income_by_month.year,
      income_by_month.month,
      monthly_income as income,
      monthly_expenses as expenses,
      monthly_income + monthly_expenses as savings
    FROM (
      SELECT
        inhaberId,
      month(buchungsDatum) as month,
        year(buchungsDatum) as year,
        sum(betrag) as monthly_income
      FROM umsatz
      WHERE betrag >= 0 and betrag < 1000000000
      GROUP BY 1, 2, 3
    ) as income_by_month INNER JOIN (
      SELECT
        inhaberId,
      month(buchungsDatum) as month,
        year(buchungsDatum) as year,
        sum(betrag) as monthly_expenses
      FROM umsatz
      WHERE betrag >= -1000000000 and betrag < 0
      GROUP BY 1, 2, 3
    ) as expenses_by_month ON
      income_by_month.inhaberId = expenses_by_month.inhaberId and
      income_by_month.year = expenses_by_month.year and
      income_by_month.month = expenses_by_month.month
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 20" do
    query = """
    SELECT
      expenses_by_month.inhaberId,
      median(monthly_income) as median_income,
      median(monthly_expenses) as median_expenses,
      median(monthly_income) + median(monthly_expenses) as median_savings
    FROM (
      SELECT
        inhaberId,
      month(buchungsDatum) as month,
        year(buchungsDatum) as year,
        sum(betrag) as monthly_income
      FROM umsatz
      WHERE betrag >= 0 and betrag < 1000000000
      GROUP BY 1, 2, 3
    ) as income_by_month INNER JOIN (
      SELECT
        inhaberId,
      month(buchungsDatum) as month,
        year(buchungsDatum) as year,
        sum(betrag) as monthly_expenses
      FROM umsatz
      WHERE betrag >= -1000000000 and betrag < 0
      GROUP BY 1, 2, 3
    ) as expenses_by_month ON
      income_by_month.inhaberId = expenses_by_month.inhaberId and
      income_by_month.year = expenses_by_month.year and
      income_by_month.month = expenses_by_month.month
    GROUP BY expenses_by_month.inhaberId
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 21" do
    query = """
    SELECT
      EXTRACT_WORDS(verwendungszweck) as word,
      count(*)
    FROM umsatz
    GROUP BY word
    ORDER BY count(*) DESC
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 22" do
    query = """
    SELECT
      EXTRACT_WORDS(verwendungszweck) as word,
      count(*)
    FROM umsatz
    WHERE verwendungszweck ILIKE '%rückzahlung%'
    GROUP BY word
    ORDER BY count(*) DESC
    LIMIT 10
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  test "sebastian 23" do
    query = """
    SELECT
      year(buchungsDatum) as year,
      month(buchungsDatum) as month,
      AVG(betrag),
      STDDEV(betrag),
      MEDIAN(betrag)
    FROM umsatz
    WHERE buchungstext ILIKE '%lohn%'
    GROUP BY year, month
    ORDER BY year DESC, month DESC
    """
    assert_compiles_successfully(query, data_source_scaffold())
  end

  # Related issue: (#2209)
  # test "sebastian 24" do
  #   query = """
  #   SELECT
  #     bucket(year2016.salary - year2015.salary by 100) as diff,
  #     count(*)
  #   FROM (
  #     SELECT
  #       inhaberId,
  #       SUM(betrag) as salary
  #     FROM umsatz
  #     WHERE
  #       buchungsDatum >= '2016-01-01' and buchungsDatum < '2017-01-01' and
  #       buchungstext ilike '%lohn%'
  #     GROUP BY inhaberId
  #   ) year2016 INNER JOIN (
  #     SELECT
  #       inhaberId,
  #       SUM(betrag) as salary
  #     FROM umsatz
  #     WHERE
  #       buchungsDatum >= '2015-01-01' and buchungsDatum < '2016-01-01' and
  #       buchungstext ilike '%lohn%'
  #     GROUP BY inhaberId
  #   ) year2015 ON year2016.inhaberId = year2015.inhaberId
  #   GROUP BY diff
  #   """
  #   assert_compiles_successfully(query, data_source_scaffold())
  # end

  defp data_source_scaffold() do
    %{
      umsatz: [
        {"inhaberId", [type: :text, uid: true]},
        {"bankverbindung.bankname", [type: :text]},
        {"bankverbindung.iban", [type: :text]},
        {"betrag", [type: :real]},
        {"bic", [type: :text]},
        {"buchungsDatum", [type: :datetime]},
        {"buchungstext", [type: :text]},
        {"kontoId", [type: :text]},
        {"name", [type: :text]},
        {"umsatzeigenschaften.anbieter", [type: :text]},
        {"umsatzeigenschaften.spezifizierung", [type: :text]},
        {"umsatzeigenschaften.unterKategorie", [type: :text]},
        {"verwendungszweck", [type: :text]},
      ],
      konto: [
        {"inhaberId", [type: :text, uid: true]},
        {"_id", [type: :text]},
        {"bankzugangId", [type: :text]},
        {"kontostand.betrag", [type: :real]},
        {"letzterUmsatz.buchungsDatum", [type: :datetime]},
        {"name", [type: :text]},
      ],
      bankzugang: [
        {"inhaberId", [type: :text, uid: true]},
        {"_id", [type: :text]},
        {"bankname", [type: :text]},
        {"blz", [type: :text]},
      ]
    }
  end

  defp vb_view() do
    """
    -- alle Konten + Bankzugänge, die ein Genossenschaftskonto haben ohne DEMO
    -- am Ende sind konto.inhaberId (VB_USER_KONTO) = bankzugang.inhaberId (VB_USER_BANK) und
    -- bankzugang._id (Zugang) = konto.bankzugangId
    SELECT
      konto.inhaberId AS VB_USER_KONTO,
      konto._id AS subKontoId,
      bankzugang.blz,
      bankzugang.bankname,
      bankzugang._id AS subBankzugangId,
      konto.letzterUmsatz.buchungsDatum
    FROM konto JOIN bankzugang ON
      bankzugang._id = konto.bankzugangId AND
      bankzugang.inhaberId = konto.inhaberId
    WHERE
      bankzugang.inhaberId = VB_USER_KONTO AND
      SUBSTRING(CAST(bankzugang.blz, text) FROM 4 FOR 1) in ('6', '9') AND
      cast(bankzugang.blz as text) <> '90090042'
    """
  end

  defp assert_compiles_successfully(query, data_source_scaffold) do
    parsed_query = Cloak.Sql.Parser.parse!(query)
    data_source = generate_data_source_config(data_source_scaffold)
    assert {:ok, _, _} = Cloak.Sql.Compiler.compile(data_source, parsed_query, nil, %{})
  end

  defp generate_data_source_config(scaffold) do
    tables = for {name, _} = table <- scaffold, into: %{}, do:
      {name, table_from_scaffold(table)}
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: tables,
    }
  end

  defp table_from_scaffold({table, column_data}) do
    table = to_string(table)
    uid_column_name = uid_column_name(column_data)
    columns = Enum.map(column_data, fn({name, params}) ->
      Cloak.DataSource.Table.column(name, Keyword.get(params, :type))
    end)
    Cloak.DataSource.Table.new(table, uid_column_name, db_name: table, columns: columns)
  end

  defp uid_column_name(columns) do
    Enum.find_value(columns, fn({name, params}) ->
      case Keyword.get(params, :uid) do
        nil -> nil
        true -> name
      end
    end)
  end
end
