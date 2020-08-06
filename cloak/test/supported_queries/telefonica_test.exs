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
