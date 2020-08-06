defmodule Cloak.Regressions.TeamBank.Test do
  use ExUnit.Case, async: true

  test "party id – dual: select *", do: assert_compiles_successfully("SELECT * FROM dual", data_source_party_id())

  test "party id - dual: type casts" do
    query = """
    select cast(true as boolean) as col_boolean
         , current_date() as col_date
         , current_timestamp() as col_timestamp
     --    , current_time() as col_time
         , cast(123456789 as integer) as col_integer
         , cast(3.141592653589 as real) as col_real
         , 'text column äöüß' as col_text
      from dual
    """

    assert_compiles_successfully(query, data_source_party_id())
  end

  test "party id - vendor counts" do
    query = """
    SELECT dd
           , vendor_name AS vendornames
           , count(*)    AS count_customers
           from aircloak.dda_endresult_vendor e
    group by vendor_name
           , order_by
           , dd
    order by order_by
    """

    assert_compiles_successfully(query, data_source_party_id())
  end

  test "party id - handset stats" do
    query = """
    SELECT handset, MIN(tmp.dd) as dd, MIN(tmp.cnt) as count_min, MAX(tmp.cnt) as count_max
    FROM (SELECT c.vendor_name || ' ' || b.model_name AS handset,netw.dd as dd,COUNT(*) as cnt
    FROM dw.cc_handset_type_properties htp
    /*  LEFT*/ JOIN dw.cc_dim_hst_property_values prv
    ON htp.hst_property_id=prv.hst_property_id
    AND htp.hst_property_value_id=prv.hst_property_value_id
    JOIN dw.cc_handset_type b
    ON htp.handset_type_id=b.handset_type_id
    JOIN dw.cc_hs_vendor c
    ON b.vendor_id=c.vendor_id
    left join aircloak.dda_avg_lte_hs netw
    on netw.handset_type_id=b.handset_type_id
    WHERE htp.hst_property_id=1024
    AND prv.value_label='JA' GROUP BY c.vendor_name || ' ' || b.model_name, dd
    HAVING COUNT ( * ) > 2000) as tmp
    GROUP BY handset
    having MIN(tmp.dd) = DATE_TRUNC('month', CURRENT_DATE())
    order by 4 desc
    """

    assert_compiles_successfully(query, data_source_party_id())
  end

  test "party id - sanity check query?" do
    query = """
    select count(*) as cnt, count_noise(*) as noise, sum(test) as summe from (
      select contract_id, count_noise(*) as test
      from dw.w_contracts c
    where system_stack_id = 11
      and order_date between DATE'2020-04-01' and DATE'2020-04-12'
    group by contract_id) t
    """

    assert_compiles_successfully(query, data_source_party_id())
  end

  test "contract id - data usage by data pack" do
    query = """
    select count(*),count(distinct uub.CONTRACT_ID )
    ,sum(FCT_UNIT_KILOBYTES)
    ,pack.Pack
    from dw.FCT_UUB_CTR_DD uub
    left join
        (select --+ parallel(12)
        distinct a.contract_id
        ,'Y'as Pack
        from dw.cc_contracts_product_features a
        join  dw.cc_product_features b on a.PRD_FEATURE_ID =b.PRD_FEATURE_ID
        join dw.cc_contracts c on a.contract_id =c.contract_id
        WHERE a.start_dt between DATE'1900-01-01'  and  DATE'2020-06-01'
        and a.end_dt between  DATE'2020-05-01' and  DATE'9999-12-31'
        and a.PRD_FEATURE_ID in (2435,2432,2449,2624,6823,2725,2194,2438,2630,2628,2726,2738,2192,2625,2205,2434,
                                 2187,2737,2626,2198,2190,2629,2437,2186,2436,2627,2623,2593,2433,2724,2450,2622)
        and a.IS_PREPAID ='Y'
        and c.SERVICE_PROVIDER_id in (104,70)
        )pack  on uub.CONTRACT_ID=pack.CONTRACT_ID
        join  dw.cc_contracts tmp   on uub.CONTRACT_ID=tmp.CONTRACT_ID
    where REV_TYPE_DESC_ID in(66)
    and tmp.SERVICE_PROVIDER_id in (104,70)
    and date_trunc('month', uub.trans_dt) = DATE'2020-05-01'
    and uub.trans_dt >= DATE'2020-05-01' and uub.trans_dt < DATE'2020-06-01'
    group by pack.Pack
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - data usage by data pack / mth" do
    query = """
    select count(*),count(distinct uub.CONTRACT_ID )
    ,sum(FCT_UNIT_KILOBYTES)
    ,pack.Pack
    ,date_trunc('month', uub.trans_dt) as month
    from dw.FCT_UUB_CTR_DD uub
    left join
        (select --+ parallel(12)
        distinct a.contract_id
        ,'Y'as Pack
        from dw.cc_contracts_product_features a
        join  dw.cc_product_features b on a.PRD_FEATURE_ID =b.PRD_FEATURE_ID
        join dw.cc_contracts c on a.contract_id =c.contract_id
        WHERE a.start_dt between DATE'1900-01-01'  and  DATE'2020-07-01'
        and a.end_dt between  DATE'2020-01-01' and  DATE'9999-12-31'
        and a.PRD_FEATURE_ID in (2435,2432,2449,2624,6823,2725,2194,2438,2630,2628,2726,2738,2192,2625,2205,2434,
                                 2187,2737,2626,2198,2190,2629,2437,2186,2436,2627,2623,2593,2433,2724,2450,2622)
        and a.IS_PREPAID ='Y'
        and c.SERVICE_PROVIDER_id in (104,70)
        )pack  on uub.CONTRACT_ID=pack.CONTRACT_ID
        join  dw.cc_contracts tmp   on uub.CONTRACT_ID=tmp.CONTRACT_ID
    where REV_TYPE_DESC_ID in(66)
    and tmp.SERVICE_PROVIDER_id in (104,70)
    and date_trunc('month', uub.trans_dt) between DATE'2020-01-01' and DATE'2020-07-01'
    and uub.trans_dt between DATE'2020-01-01' and DATE'2020-07-01'
    group by pack.Pack,date_trunc('month', uub.trans_dt)
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - product desc" do
    query = """
    select product_id, product_desc
                    from dw.cc_products
                   where lower(PRODUCT_DESC) LIKE '%data%spot%'
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - active contract count for certain date range and data class" do
    query = """
    select --+ parallel (a 32) parallel (b 32)
                  count(*)
             from dw.cc_contracts_dm a
                , ( -- temp1: select product_ids
                  select product_id, product_desc
                    from dw.cc_products
                   where lower(PRODUCT_DESC) LIKE '%data%spot%'
                  ) b
            where a.service_provider_id  = 1
              and A.CONTRACT_TYPE_ID     = 1
              and is_active              = 'Y'
    --          and start_dt              <= trunc(sysdate-32)
    --          and end_dt                 > trunc(sysdate-31)
              and a.market_type_id      <> 2
              and a.service_provider_id  = 1
              and a.PRODUCT_ID_CURRENT   = b.product_id
    --          and activation_dt         <= trunc(sysdate-32)
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - gigabyte bucket sized usage count" do
    query = """
    select bucket(total_usage_in_kilobytes by 1000000 align upper) as bucket_1gb
       , count(*) as cnt
    from
      (
        -- usage per contract_id
        select ub.contract_id,
          sum(fct_unit_kilobytes) as total_usage_in_kilobytes
        from (
           select A.CONTRACT_ID
             from dw.cc_contracts_dm a
                , ( -- temp1: select product_ids
                  select product_id, product_desc
                    from dw.cc_products
                   where lower(PRODUCT_DESC) LIKE '%data%spot%'
                  ) b
            where a.service_provider_id  = 1
              and A.CONTRACT_TYPE_ID     = 1
              and is_active              = 'Y'
    --          and start_dt              <= trunc(sysdate-32)
    --          and end_dt                 > trunc(sysdate-31)
              and a.market_type_id      <> 2
              and a.service_provider_id  = 1
              and a.PRODUCT_ID_CURRENT   = b.product_id
    --          and activation_dt         <= trunc(sysdate-32)
             ) ub
       join dw.fct_uub_ctr_dd fct
         on fct.contract_id = ub.contract_id
        and   fct.rev_type_desc_id IN (334, 66)
        and fct.trans_dt = cast(current_date() - interval 'P32D' as date)
        group by ub.contract_id
        order by 1
      ) upc
    group by bucket(total_usage_in_kilobytes by 1000000 align upper)
    order by 1
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - upper aligned gigabyte bucket sized usage count with noise" do
    query = """
    select
      bucket(total_usage_in_kilobytes by 1000000 align upper) AS u1gb
       , count(*) as cnt
       , count_noise(*) as cnt_noise
    from
      (
      -- volume per contract_id
        select
          ub.contract_id,
          sum(fct_unit_kilobytes) as total_usage_in_kilobytes
        from ( -- select contract_ids
           select a.contract_id
             from dw.cc_contracts_dm a
                , ( -- temp1: select product_ids
                  select product_id, product_desc
                    from dw.cc_products
                   where lower(PRODUCT_DESC) LIKE '%data%spot%'
                  ) b
            where a.service_provider_id  = 1
              and A.CONTRACT_TYPE_ID     = 1
              and is_active              = 'Y'
    --          and start_dt              <= trunc(sysdate-32)
    --          and end_dt                 > trunc(sysdate-31)
    --          and activation_dt         <= trunc(sysdate-32)
              and a.market_type_id      <> 2
              and a.service_provider_id  = 1
              and a.PRODUCT_ID_CURRENT   = b.product_id
             ) ub
       join dw.fct_uub_ctr_dd fct
         on fct.contract_id = ub.contract_id
        and   fct.rev_type_desc_id IN (334, 66)
        and fct.trans_dt = cast(current_date() - interval 'P32D' as date)
        group by ub.contract_id
        order by 1
      ) upc
    group by bucket(total_usage_in_kilobytes by 1000000 align upper)
    order by 1
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - total data usage, contract count by month and pack type" do
    query = """
    select
    count(distinct CONTRACT_ID ),
    sum(FCT_UNIT_KILOBYTES) ,
    new.month,
    new.Pack
    from
    (
    select uub.CONTRACT_ID
    ,sum(FCT_UNIT_KILOBYTES) as FCT_UNIT_KILOBYTES
    ,pack.Pack
    ,date_trunc('month', uub.trans_dt) as month
    from dw.FCT_UUB_CTR_DD uub
    left join
        (select --+ parallel(12)
        distinct a.contract_id
        ,'Y'as Pack
        from dw.cc_contracts_product_features a
        join  dw.cc_product_features b on a.PRD_FEATURE_ID =b.PRD_FEATURE_ID
        join dw.cc_contracts c on a.contract_id =c.contract_id
        WHERE a.start_dt between DATE'1900-01-01'  and  DATE'2020-07-01'
        and a.end_dt between  DATE'2020-01-01' and  DATE'9999-12-31'
        and a.PRD_FEATURE_ID in (2435,2432,2449,2624,6823,2725,2194,2438,2630,2628,2726,2738,2192,2625,2205,2434,
                                 2187,2737,2626,2198,2190,2629,2437,2186,2436,2627,2623,2593,2433,2724,2450,2622)
        and a.IS_PREPAID ='Y'
        and c.SERVICE_PROVIDER_id in (104,70)
        )pack  on uub.CONTRACT_ID=pack.CONTRACT_ID
        join  dw.cc_contracts tmp   on uub.CONTRACT_ID=tmp.CONTRACT_ID
    where REV_TYPE_DESC_ID in(66)
    and tmp.SERVICE_PROVIDER_id in (104,70)
    and date_trunc('month', uub.trans_dt) between DATE'2020-01-01' and DATE'2020-07-01'
    and uub.trans_dt between DATE'2020-01-01' and DATE'2020-07-01'
    group by pack.Pack,date_trunc('month', uub.trans_dt),uub.CONTRACT_ID
    ) new
    where new.month = DATE'2020-01-01'
    and new.month = DATE'2020-02-01'
    and new.month = DATE'2020-03-01'
    and new.month = DATE'2020-04-01'
    and new.month = DATE'2020-05-01'
    and new.month = DATE'2020-06-01'
    group by month,Pack
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - query on dual about ilike usage" do
    query = """
    select *  from dual where dummy ilike '%X%'
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - count transaction dates by week" do
    query = """
    SELECT count (uub.TRANS_DT),
      DATE_TRUNC ('WEEK', MAX (  uub.TRANS_DT)) AS max_trans_dt
                            FROM dw.FCT_UUB_CTR_DD uub
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - count transaction dates by duration and feature id classification" do
    query = """
    SELECT COUNT (c.contract_id) as count_contracts,
                      DATE_TRUNC ('WEEK', uub.max_trans_dt) as  max_trans_week,
                      DATE_TRUNC ('WEEK', cpf.start_dt)  as start_dt_week,
                       cpf.start_dt,
                       uub.max_trans_dt-cpf.start_dt,
                       CASE WHEN cpf.PRD_FEATURE_ID = 2588 THEN 1 ELSE 0 END
                  FROM dw.cc_contracts c
                       LEFT JOIN
                       (  SELECT uub.contract_id,
                           MAX (  uub.TRANS_DT) AS max_trans_dt
                            FROM dw.FCT_UUB_CTR_DD uub
                        GROUP BY uub.contract_id) uub
                          ON c.CONTRACT_ID = uub.CONTRACT_ID
                       LEFT JOIN dw.cc_contracts_product_features cpf
                          ON c.contract_id = cpf.contract_id
                 WHERE     1 = 1
                       AND c.ACTIVATION_DT >= CAST ('2020-01-01' AS DATE)
                       AND c.ACTIVATION_DT < CAST ('2021-01-01' AS DATE)
              GROUP BY                     DATE_TRUNC ('WEEK', uub.max_trans_dt) ,
                      DATE_TRUNC ('WEEK', cpf.start_dt) ,
                       cpf.start_dt,
                       uub.max_trans_dt-cpf.start_dt,
                       CASE WHEN cpf.PRD_FEATURE_ID = 2588 THEN 1 ELSE 0 END,
                       uub.max_trans_dt,
                       cpf.start_dt
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - count contracts by whether active for x days and promo" do
    query = """
    select
     count ( sub.count_contracts) ,
     max_trans_week,
     start_dt_week,
     lief_breits_x_tage,
     promo
     from
     (
     SELECT COUNT (c.contract_id) as count_contracts,
                      DATE_TRUNC ('WEEK', uub.max_trans_dt) as  max_trans_week,
                      DATE_TRUNC ('WEEK', cpf.start_dt)  as start_dt_week,
                       cpf.start_dt,
                       uub.max_trans_dt-cpf.start_dt as lief_breits_x_tage,
                       CASE WHEN cpf.PRD_FEATURE_ID = 2588 THEN 1 ELSE 0 END as promo
                  FROM dw.cc_contracts c
                       LEFT JOIN
                       (  SELECT uub.contract_id,
                           MAX (  uub.TRANS_DT) AS max_trans_dt
                            FROM dw.FCT_UUB_CTR_DD uub
                        GROUP BY uub.contract_id) uub
                          ON c.CONTRACT_ID = uub.CONTRACT_ID
                       LEFT JOIN dw.cc_contracts_product_features cpf
                          ON c.contract_id = cpf.contract_id
                 WHERE     1 = 1
                       AND c.ACTIVATION_DT >= CAST ('2020-01-01' AS DATE)
                       AND c.ACTIVATION_DT < CAST ('2021-01-01' AS DATE)
              GROUP BY                     DATE_TRUNC ('WEEK', uub.max_trans_dt) ,
                      DATE_TRUNC ('WEEK', cpf.start_dt) ,
                       cpf.start_dt,
                       uub.max_trans_dt-cpf.start_dt,
                       CASE WHEN cpf.PRD_FEATURE_ID = 2588 THEN 1 ELSE 0 END,
                       uub.max_trans_dt,
                       cpf.start_dt) sub
                       group by max_trans_week,
     start_dt_week,
     lief_breits_x_tage,
     promo
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - count contracts by start and end date and whether due to promo" do
    query = """
    SELECT COUNT (c.contract_id),
             uub.max_trans_dt_week,
             cpf.start_dt_week,
             'yes' AS promo
        FROM dw.cc_contracts c
             INNER JOIN
             (SELECT cpf.contract_id,
                     date_trunc ('WEEK', cpf.start_dt) AS start_dt_week
                FROM dw.cc_contracts_product_features cpf
               WHERE cpf.PRD_FEATURE_ID = 2588) cpf
                ON c.contract_id = cpf.contract_id
             LEFT JOIN
             (  SELECT uub.contract_id,
                       DATE_TRUNC ('WEEK', MAX (uub.TRANS_DT)) AS max_trans_dt_week
                  FROM dw.FCT_UUB_CTR_DD uub
              GROUP BY uub.contract_id) uub
                ON c.CONTRACT_ID = uub.CONTRACT_ID
       WHERE     1 = 1
          AND c.ACTIVATION_DT BETWEEN DATE '2020-01-01' AND DATE '2020-09-01'
          AND c.SYSTEM_STACK_ID = 2
          AND c.service_provider_id =1
          AND c.IS_PREPAID = 'Y'
    GROUP BY uub.max_trans_dt_week, cpf.start_dt_week
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  test "contract id - contract termination count by start week" do
    query = """
    SELECT COUNT (c.contract_id),
             uub.max_trans_dt_week,
             date_trunc ('month', cpf.start_dt) AS start_dt_month,
             'yes' AS promo
        FROM dw.cc_contracts c
             INNER JOIN
              dw.cc_contracts_product_features cpf
                ON c.contract_id = cpf.contract_id
                and cpf.PRD_FEATURE_ID = 2588
                AND cpf.SYSTEM_STACK_ID = 2
             LEFT JOIN
             (  SELECT uub.contract_id,
                   DATE_TRUNC ('week', MAX (uub.TRANS_DT)) AS max_trans_dt_week
                  FROM dw.FCT_UUB_CTR_DD uub
                WHERE     1 = 1
          AND uub.TRANS_DT BETWEEN DATE '2020-05-01' AND DATE '2020-09-01'
              and TA_SOURCE_SYSTEM  in  ( 'IN', 'IXD_PRE')
              GROUP BY uub.contract_id) uub
                ON c.CONTRACT_ID = uub.CONTRACT_ID
       WHERE     1 = 1
          AND c.ACTIVATION_DT BETWEEN DATE '2020-01-01' AND DATE '2020-09-01'
          AND c.SYSTEM_STACK_ID = 2
          AND c.service_provider_id =1
          AND c.IS_PREPAID = 'Y'
    GROUP BY uub.max_trans_dt_week, date_trunc ('month', cpf.start_dt)
    """

    assert_compiles_successfully(query, data_source_contract_id())
  end

  defp data_source_contract_id(),
    do: %{
      "DUAL" => [
        {"DUMMY", [type: :text]}
      ],
      "DW.CC_CONTRACTS_DM" => [
        {"CONTRACT_ID", [type: :integer, uid: true]},
        {"ACADEMIC_TITLE_NAME", [type: :text]},
        {"ACTIVATION_DT", [type: :date]},
        {"BILLING_ACCOUNT_ID", [type: :integer]},
        {"BIRTH_DATE", [type: :date]},
        {"CARRIER_ID", [type: :integer]},
        {"CITYZONE_ID_CURRENT", [type: :integer]},
        {"CITYZONE_ID_INIT", [type: :integer]},
        {"CLIENT_ID", [type: :integer]},
        {"CNT_PARTY_CHG", [type: :integer]},
        {"CONSENT_FLAG_DATA", [type: :text]},
        {"CONSENT_FLAG_EMAIL", [type: :text]},
        {"CONSENT_FLAG_FAX", [type: :text]},
        {"CONSENT_FLAG_MAIL", [type: :text]},
        {"CONSENT_FLAG_PHONE", [type: :text]},
        {"CONSENT_FLAG_SMS", [type: :text]},
        {"CONSENT_FLAG_WEB", [type: :text]},
        {"CONTRACT_PERIOD_ID", [type: :integer]},
        {"CONTRACT_TYPE_ID", [type: :integer]},
        {"COUNTRY_ID_NATIONALITY", [type: :integer]},
        {"CTT_ID", [type: :integer]},
        {"EMAIL_ADDRESS", [type: :text]},
        {"END_DT", [type: :date]},
        {"END_REASON_ID", [type: :integer]},
        {"EXPORT_CARRIER_ID", [type: :integer]},
        {"FW_CONTRACT_ID", [type: :integer]},
        {"FW_CONTRACT_NO", [type: :integer]},
        {"HANDSET_TYPE_DESC", [type: :text]},
        {"HANDSET_TYPE_ID_CURRENT_1", [type: :integer]},
        {"HANDSET_TYPE_ID_CURRENT_2", [type: :integer]},
        {"HANDSET_TYPE_ID_CURRENT_3", [type: :integer]},
        {"HANDSET_TYPE_ID_CURRENT_4", [type: :integer]},
        {"HANDSET_TYPE_ID_CURRENT_5", [type: :integer]},
        {"HANDSET_TYPE_ID_CURRENT_6", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_1", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_2", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_3", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_4", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_5", [type: :integer]},
        {"HANDSET_TYPE_ID_INIT_6", [type: :integer]},
        {"HZ_GIS_R2_CURRENT", [type: :integer]},
        {"HZ_GIS_R2_INIT", [type: :integer]},
        {"HZ_GIS_X_CURRENT", [type: :integer]},
        {"HZ_GIS_X_INIT", [type: :integer]},
        {"HZ_GIS_Y_CURRENT", [type: :integer]},
        {"HZ_GIS_Y_INIT", [type: :integer]},
        {"HZ_LAC_CURRENT", [type: :text]},
        {"HZ_LAC_INIT", [type: :text]},
        {"HZ_MSISDN_CURRENT", [type: :text]},
        {"HZ_MSISDN_INIT", [type: :text]},
        {"IMSI", [type: :text]},
        {"IMSI_2", [type: :text]},
        {"IMSI_3", [type: :text]},
        {"IMSI_4", [type: :text]},
        {"IMSI_5", [type: :text]},
        {"IMSI_6", [type: :text]},
        {"IS_ACTIVE", [type: :text]},
        {"IS_CURRENT", [type: :text]},
        {"IS_PREPAID", [type: :text]},
        {"IS_SIM_ONLY", [type: :text]},
        {"LAST_PARTY_CHG_DT", [type: :date]},
        {"LOCAL_LAC_CURRENT", [type: :text]},
        {"LOCAL_LAC_INIT", [type: :text]},
        {"MARKET_TYPE_ID", [type: :integer]},
        {"MNP_IMPORT_STATE_ID", [type: :integer]},
        {"MNP_IMPORT_TYPE_ID", [type: :integer]},
        {"MSISDN_VOICE_CURRENT", [type: :text]},
        {"OFFICE_LAC_CURRENT", [type: :text]},
        {"OFFICE_LAC_INIT", [type: :text]},
        {"OFFICE_PREFIX_CURRENT", [type: :text]},
        {"OFFICE_PREFIX_INIT", [type: :text]},
        {"OI_SPTARIFFCODE_CURRENT", [type: :integer]},
        {"OI_SPTARIFFCODE_INIT", [type: :integer]},
        {"OLD_HZ_ADDRESS_ID_CURRENT", [type: :integer]},
        {"OLD_HZ_ADDRESS_ID_INIT", [type: :integer]},
        {"PART", [type: :integer]},
        {"PARTY_ID", [type: :integer]},
        {"PORTING_DT", [type: :date]},
        {"PRODUCT_ID_CURRENT", [type: :integer, key: :product_id]},
        {"PRODUCT_ID_INIT", [type: :integer]},
        {"PRODUCT_START_DT_CURRENT", [type: :date]},
        {"PROVIDER_ID", [type: :integer]},
        {"RESELLER_ID", [type: :integer]},
        {"SALES_CHANNEL_ID_EXEC", [type: :integer]},
        {"SALES_CHANNEL_ID_INIT", [type: :integer]},
        {"SERVICE_PROVIDER_ID", [type: :integer]},
        {"SIMICCID", [type: :text]},
        {"SIMICCID_2", [type: :text]},
        {"SIMICCID_3", [type: :text]},
        {"SIMICCID_4", [type: :text]},
        {"SIMICCID_5", [type: :text]},
        {"SIMICCID_6", [type: :text]},
        {"SOURCE_ID_POC", [type: :text]},
        {"SOURCE_SUBSCRIPTION_ID", [type: :text]},
        {"SOURCE_SYSTEM_CODE_POC", [type: :text]},
        {"START_DT", [type: :date]},
        {"START_REASON_ID", [type: :integer]},
        {"SYSTEM_STACK_ID", [type: :integer]},
        {"TITLE_NAME", [type: :text]}
      ],
      "DW.CC_PRODUCTS" => [
        {"DATA_BILLING_TYPE_ID", [type: :integer]},
        {"DSL_BANDWIDTH_ID", [type: :integer]},
        {"HAS_CITY_OPTION", [type: :text]},
        {"HAS_FREIZEIT_OPTION", [type: :text]},
        {"HAS_INTERNATIONAL_OPTION", [type: :text]},
        {"HAS_LOCAL_OPTION", [type: :text]},
        {"HAS_MOBILE_OPTION", [type: :text]},
        {"HAS_O2_OPTION", [type: :text]},
        {"HAS_OFFICE_OPTION", [type: :text]},
        {"HAS_POWER_UPLOAD", [type: :text]},
        {"HAS_RECURRING_CHARGES", [type: :text]},
        {"HAS_SMS_OPTION", [type: :text]},
        {"HAS_SPEED_PING", [type: :text]},
        {"IS_FREECARD", [type: :text]},
        {"IS_HOMEZONE_PRODUCT", [type: :text]},
        {"PRODUCT_CLOCK_RATE_CODE", [type: :text]},
        {"PRODUCT_CLOCK_RATE_DESC", [type: :text]},
        {"PRODUCT_CLOCK_RATE_ID", [type: :integer]},
        {"PRODUCT_DESC", [type: :text]},
        {"PRODUCT_FAMILY_CODE", [type: :text]},
        {"PRODUCT_FAMILY_DESC", [type: :text]},
        {"PRODUCT_FAMILY_ID", [type: :integer]},
        {"PRODUCT_FORECAST_GROUP_CODE", [type: :text]},
        {"PRODUCT_FORECAST_GROUP_DESC", [type: :text]},
        {"PRODUCT_FORECAST_GROUP_ID", [type: :integer]},
        {"PRODUCT_ID", [type: :integer, key: :product_id]},
        {"PRODUCT_MULTI_CODE", [type: :text]},
        {"PRODUCT_MULTI_DESC", [type: :text]},
        {"PRODUCT_MULTI_ID", [type: :integer]},
        {"PRODUCT_PAYMENT_CODE", [type: :text]},
        {"PRODUCT_PAYMENT_DESC", [type: :text]},
        {"PRODUCT_PAYMENT_ID", [type: :integer]},
        {"PRODUCT_REPORT_GROUP_ID", [type: :integer]},
        {"PRODUCT_TARIFF_GROUP_ID", [type: :integer]},
        {"PRODUCT_TYPE_ID", [type: :integer]},
        {"SA_DELETE_DATE", [type: :date]},
        {"SA_END_DATE", [type: :date]},
        {"SA_OPERATION", [type: :text]},
        {"SA_REFERENCE_DATE", [type: :date]},
        {"SA_ROWID", [type: :text]},
        {"SA_START_DATE", [type: :date]},
        {"SA_VERSION_ID", [type: :integer]},
        {"SA_VERSION_INS", [type: :integer]},
        {"SA_VERSION_REF", [type: :integer]},
        {"SA_VERSION_UPD", [type: :integer]},
        {"SIM_ONLY_CODE", [type: :text]},
        {"SIM_ONLY_ID", [type: :integer]},
        {"VOICE_BILLING_TYPE_ID", [type: :integer]}
      ],
      "DW.FCT_UUB_CTR_DD" => [
        {"CONTRACT_ID", [type: :integer, uid: true]},
        {"CORRIDOR_PLAN_ID", [type: :integer]},
        {"FCT_CNT_CDR", [type: :integer]},
        {"FCT_COSTS_CENTS", [type: :integer]},
        {"FCT_DISCOUNTS_CENTS", [type: :integer]},
        {"FCT_REVENUE_CENTS", [type: :integer]},
        {"FCT_UNIT_KILOBYTES", [type: :integer]},
        {"FCT_UNIT_NUMBERS", [type: :integer]},
        {"FCT_UNIT_SECONDS", [type: :integer]},
        {"JURISDICTION", [type: :integer]},
        {"REMOTE_COUNTRY_ID", [type: :integer]},
        {"REMOTE_NETWORK_ID", [type: :integer]},
        {"REMOTE_NETWORK_TYPE_ID", [type: :integer]},
        {"REV_TYPE_DESC_ID", [type: :integer]},
        {"ROAMING_PROVIDER_ID", [type: :integer]},
        {"SYSTEM_STACK_ID", [type: :integer]},
        {"TA_CHARGABLE_UNITS", [type: :integer]},
        {"TA_FCT_UNIT_KILOBYTES", [type: :integer]},
        {"TA_RATED_UNITS", [type: :integer]},
        {"TA_SOURCE_SYSTEM", [type: :text]},
        {"TA_UUB_LOAD_ID", [type: :integer]},
        {"TRANS_DT", [type: :date]},
        {"TYPE_ID_USG", [type: :integer]}
      ],
      "DW.CC_CONTRACTS_PRODUCT_FEATURES" => [
        {"CONTRACT_ID", [type: :integer, uid: true]},
        {"ACTIVATION_SOURCE_ID", [type: :integer]},
        {"END_DT", [type: :date]},
        {"IS_CURRENT", [type: :text]},
        {"IS_PREPAID", [type: :text]},
        {"MIN_PRD_FEATURE_DURATION", [type: :integer]},
        {"PART", [type: :integer]},
        {"PRD_FEATURE_ID", [type: :integer, key: :prd_feature_id]},
        {"RESELLER_ID", [type: :integer]},
        {"SOURCE", [type: :text]},
        {"SOURCE_ID", [type: :text]},
        {"START_DT", [type: :date]},
        {"SYSTEM_STACK_ID", [type: :integer]},
        {"TESTEND_DT", [type: :date]}
      ],
      "DW.CC_CONTRACTS" => [
        {"CONTRACT_ID", [type: :integer, uid: true]},
        {"ACTIVATION_DT", [type: :date]},
        {"BILLING_ACCOUNT_ID", [type: :integer]},
        {"BRAND_ID", [type: :integer]},
        {"CONTRACT_PERIOD_ID", [type: :integer]},
        {"CONTRACT_TYPE_ID", [type: :integer]},
        {"CREATE_DT", [type: :date]},
        {"FIRST_CALL_DT", [type: :date]},
        {"FW_CONTRACT_ID", [type: :integer]},
        {"HANDSET_TYPE_DESC", [type: :text]},
        {"HVT_ID", [type: :text]},
        {"IS_PREPAID", [type: :text]},
        {"IS_SIM_ONLY", [type: :text]},
        {"LABEL_ID", [type: :integer]},
        {"MARKET_TYPE_ID", [type: :integer]},
        {"MSISDN_VOICE", [type: :text]},
        {"ORDER_DATE", [type: :date]},
        {"ORDER_DT", [type: :date]},
        {"PARTY_ID", [type: :integer]},
        {"RESELLER_ID", [type: :integer]},
        {"SERVICE_PROVIDER_ID", [type: :integer]},
        {"SIMICCID", [type: :text]},
        {"SOURCE_SUBSCRIPTION_ID", [type: :text]},
        {"SYSTEM_STACK_ID", [type: :integer]},
        {"TESTCARD_ID", [type: :integer]}
      ],
      "DW.CC_PRODUCT_FEATURES" => [
        {"DESCRIPTION", [type: :text]},
        {"IR_DATA_PACK_DESC", [type: :text]},
        {"IR_DATA_PACK_ID", [type: :integer]},
        {"IR_VOICE_OPTION_DESC", [type: :text]},
        {"IR_VOICE_OPTION_ID", [type: :integer]},
        {"IS_MOBILE_DATA_BASIC_CHARGE", [type: :text]},
        {"IS_MOBILE_DATA_FEATURE", [type: :text]},
        {"PO_REPORT_GROUP_ID", [type: :integer]},
        {"PRD_FEATURE_GROUP_ID", [type: :integer]},
        {"PRD_FEATURE_ID", [type: :integer, key: :prd_feature_id]},
        {"PRD_FEATURE_SUB_GROUP_ID", [type: :integer]},
        {"PRD_FEATURE_TYPE_ID", [type: :integer]},
        {"PRD_FEATURE_TYPE_NAME", [type: :text]},
        {"PRF_FEATURE_GROUP_NAME", [type: :text]},
        {"PRF_FEATURE_SUB_GROUP_NAME", [type: :text]},
        {"USED_BY_BUS_REPORT_FLAG", [type: :text]}
      ]
    }

  defp data_source_party_id(),
    do: %{
      "dual" => [],
      "AIRCLOAK.DDA_ENDRESULT_VENDOR" => [
        {"PARTY_ID", [type: :integer, uid: true]},
        {"DD", [type: :date]},
        {"ORDER_BY", [type: :integer]},
        {"VENDOR_NAME", [type: :text]}
      ],
      "DW.CC_HANDSET_TYPE_PROPERTIES" => [
        {"HANDSET_TYPE_ID", [type: :integer, key: :handset_type_id]},
        {"HST_PROPERTY_ID", [type: :integer, key: :handset_property_id]},
        {"HST_PROPERTY_VALUE", [type: :text]},
        {"HST_PROPERTY_VALUE_ID", [type: :integer, key: :handset_property_value_id]},
        {"SA_DELETE_DATE", [type: :date]},
        {"SA_END_DATE", [type: :date]},
        {"SA_REFERENCE_DATE", [type: :date]},
        {"SA_ROWID", [type: :text]},
        {"SA_START_DATE", [type: :date]},
        {"SA_VERSION_ID", [type: :integer]},
        {"SA_VERSION_INS", [type: :integer]},
        {"SA_VERSION_UPD", [type: :integer]},
        {"TYPE", [type: :integer]}
      ],
      "DW.CC_HANDSET_TYPE" => [
        {"AMR_HALFRATE", [type: :text]},
        {"BLUETOOTH_ENABLED", [type: :text]},
        {"BROWSER_ENABLED", [type: :text]},
        {"CAMERA_MP", [type: :text]},
        {"CREATE_DT", [type: :date]},
        {"CREATE_USER", [type: :text]},
        {"EDGE_ENABLED", [type: :text]},
        {"EMAIL_CLIENT_ENABLED", [type: :text]},
        {"ENH_HALFRATE", [type: :text]},
        {"GPRS_ENABLED", [type: :text]},
        {"GPS_ENABLED", [type: :text]},
        {"HANDSET_CLASS_REF", [type: :text]},
        {"HANDSET_TYPE_ID", [type: :integer, key: :handset_type_id]},
        {"HAS_CAMERA_BUILT_IN", [type: :text]},
        {"HAS_CAMERA_CLIPPED_ON", [type: :text]},
        {"HAS_COLORDISPLAY", [type: :text]},
        {"HAS_POLYPHONRING", [type: :text]},
        {"HSDPA_ENABLED", [type: :text]},
        {"HSUPA_ENABLED", [type: :text]},
        {"HW_REALTON", [type: :text]},
        {"HW_TYPE", [type: :text]},
        {"IR_ENABLED", [type: :text]},
        {"IS_SMARTPHONE", [type: :text]},
        {"JAVA_ENABLED", [type: :text]},
        {"LAUNCH_DT", [type: :date]},
        {"MMS_ENABLED", [type: :text]},
        {"MODEL_NAME", [type: :text]},
        {"MODIFY_DT", [type: :date]},
        {"MODIFY_USER", [type: :text]},
        {"O2_ACTIVE_DEVICE_ENABLED", [type: :text]},
        {"OPERATING_SYSTEM", [type: :text]},
        {"OTA_ABILITY", [type: :text]},
        {"QWERTY_ENABLED", [type: :text]},
        {"SA_DELETE_DATE", [type: :date]},
        {"SA_END_DATE", [type: :date]},
        {"SA_OPERATION", [type: :text]},
        {"SA_REFERENCE_DATE", [type: :date]},
        {"SA_ROWID", [type: :text]},
        {"SA_START_DATE", [type: :date]},
        {"SA_VERSION_ID", [type: :integer]},
        {"SA_VERSION_INS", [type: :integer]},
        {"SA_VERSION_REF", [type: :integer]},
        {"SA_VERSION_UPD", [type: :integer]},
        {"SMS_WAP_LINK_ENABLED", [type: :text]},
        {"SYNC_ML_ENABLED", [type: :text]},
        {"TOUCHSCREEN_ENABLED", [type: :text]},
        {"UMTS_ENABLED", [type: :text]},
        {"VENDOR_ID", [type: :integer, key: :vendor_id]},
        {"VIDEO_STREAM", [type: :text]},
        {"WAP_ENABLED", [type: :text]},
        {"WAP_PUSH_ENABLED", [type: :text]},
        {"WAP_VERSION", [type: :text]}
      ],
      "DW.CC_HS_VENDOR" => [
        {"CREATE_DT", [type: :date]},
        {"CREATE_USER", [type: :text]},
        {"MODIFY_DT", [type: :date]},
        {"MODIFY_USER", [type: :text]},
        {"SA_DELETE_DATE", [type: :date]},
        {"SA_END_DATE", [type: :date]},
        {"SA_OPERATION", [type: :text]},
        {"SA_REFERENCE_DATE", [type: :date]},
        {"SA_ROWID", [type: :text]},
        {"SA_START_DATE", [type: :date]},
        {"SA_VERSION_ID", [type: :integer]},
        {"SA_VERSION_INS", [type: :integer]},
        {"SA_VERSION_REF", [type: :integer]},
        {"SA_VERSION_UPD", [type: :integer]},
        {"VENDOR_ID", [type: :integer, key: :vendor_id]},
        {"VENDOR_NAME", [type: :text]}
      ],
      "AIRCLOAK.DDA_AVG_LTE_HS" => [
        {"PARTY_ID", [type: :integer, uid: true]},
        {"CONTRACT_ID", [type: :integer]},
        {"DD", [type: :date]},
        {"HANDSET_TYPE_ID", [type: :integer, key: :handset_type_id]}
      ],
      "DW.CC_DIM_HST_PROPERTY_VALUES" => [
        {"CREATE_DT", [type: :date]},
        {"CREATE_USER", [type: :text]},
        {"DESCRIPTION", [type: :text]},
        {"HST_PROPERTY_ID", [type: :integer, key: :handset_property_id]},
        {"HST_PROPERTY_VALUE_ID", [type: :integer, keys: :handset_property_value_id]},
        {"IS_ACTIVE", [type: :text]},
        {"MODIFY_DT", [type: :date]},
        {"MODIFY_USER", [type: :text]},
        {"SA_DELETE_DATE", [type: :date]},
        {"SA_END_DATE", [type: :date]},
        {"SA_REFERENCE_DATE", [type: :date]},
        {"SA_ROWID", [type: :text]},
        {"SA_START_DATE", [type: :date]},
        {"SA_VERSION_ID", [type: :integer]},
        {"SA_VERSION_INS", [type: :integer]},
        {"SA_VERSION_UPD", [type: :integer]},
        {"SORT_ORDER", [type: :integer]},
        {"VALUE_LABEL", [type: :text]}
      ],
      "DW.W_CONTRACTS" => [
        {"PARTY_ID", [type: :integer, uid: true]},
        {"ACTIVATION_DT", [type: :date]},
        {"BILLING_ACCOUNT_ID", [type: :integer]},
        {"BRAND_ID", [type: :integer]},
        {"CONTRACT_ID", [type: :integer]},
        {"CONTRACT_PERIOD_ID", [type: :integer]},
        {"CONTRACT_TYPE_ID", [type: :integer]},
        {"CREATE_DT", [type: :date]},
        {"FIRST_CALL_DT", [type: :date]},
        {"FW_CONTRACT_ID", [type: :integer]},
        {"HANDSET_TYPE_DESC", [type: :text]},
        {"HVT_ID", [type: :text]},
        {"IS_PREPAID", [type: :text]},
        {"IS_SIM_ONLY", [type: :text]},
        {"LABEL_ID", [type: :integer]},
        {"MARKET_TYPE_ID", [type: :integer]},
        {"MSISDN_VOICE", [type: :text]},
        {"ORDER_DATE", [type: :date]},
        {"ORDER_DT", [type: :date]},
        {"RESELLER_ID", [type: :integer]},
        {"SERVICE_PROVIDER_ID", [type: :integer]},
        {"SIMICCID", [type: :text]},
        {"SOURCE_SUBSCRIPTION_ID", [type: :text]},
        {"SYSTEM_STACK_ID", [type: :integer]},
        {"TESTCARD_ID", [type: :integer]}
      ]
    }

  defp assert_compiles_successfully(query, data_source_scaffold) do
    parsed_query = Cloak.Sql.Parser.parse!(query)
    data_source = generate_data_source_config(data_source_scaffold)
    assert {:ok, _} = Cloak.Sql.Compiler.compile(parsed_query, nil, data_source, nil, %{})
  end

  defp generate_data_source_config(scaffold) do
    tables = for {name, _} = table <- scaffold, into: %{}, do: {name, table_from_scaffold(table)}

    %{
      name: "telefonic_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: tables
    }
  end

  defp table_from_scaffold({table, column_data}) do
    table = to_string(table)
    uid_column_name = uid_column_name(column_data)

    columns =
      Enum.map(column_data, fn {name, params} ->
        Cloak.DataSource.Table.column(name, Keyword.get(params, :type))
      end)

    keys = keys_from_columns(column_data)

    Cloak.DataSource.Table.new(table, uid_column_name, db_name: table, columns: columns, keys: keys)
  end

  defp uid_column_name(columns) do
    Enum.find_value(columns, fn {name, params} ->
      case Keyword.get(params, :uid) do
        nil -> nil
        true -> name
      end
    end)
  end

  defp keys_from_columns(columns),
    do:
      columns
      |> Enum.filter(fn {_column_name, options} -> Keyword.has_key?(options, :key) end)
      |> Enum.map(fn {column_name, options} -> {column_name, Keyword.get(options, :key)} end)
      |> Enum.into(%{})
end
