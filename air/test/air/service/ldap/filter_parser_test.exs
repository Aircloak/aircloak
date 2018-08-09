defmodule Air.Service.LDAP.FilterParser.Test do
  use ExUnit.Case, async: true

  alias Air.Service.LDAP.FilterParser

  test "presence filter" do
    assert FilterParser.parse("(field=*)") == {:ok, :eldap.present('field')}
  end

  test "equality filter" do
    assert FilterParser.parse("(field=value)") == {:ok, :eldap.equalityMatch('field', 'value')}
  end

  test "gteq filter" do
    assert FilterParser.parse("(field>=value)") == {:ok, :eldap.greaterOrEqual('field', 'value')}
  end

  test "lteq filter" do
    assert FilterParser.parse("(field<=value)") == {:ok, :eldap.lessOrEqual('field', 'value')}
  end

  test "approximate match filter" do
    assert FilterParser.parse("(field~=value)") == {:ok, :eldap.approxMatch('field', 'value')}
  end

  describe "substring filter" do
    test "*a*b*" do
      assert FilterParser.parse("(field=*a*b*)") == {:ok, :eldap.substrings('field', any: 'a', any: 'b')}
    end

    test "a*c*b" do
      assert FilterParser.parse("(field=a*b*c)") ==
               {:ok, :eldap.substrings('field', initial: 'a', any: 'b', final: 'c')}
    end
  end

  describe "extensible match filter" do
    test "(field:=value)" do
      assert FilterParser.parse("(field:=value)") == {:ok, :eldap.extensibleMatch('value', type: 'field')}
    end

    test "(field:rule:=value)" do
      assert FilterParser.parse("(field:rule:=value)") ==
               {:ok, :eldap.extensibleMatch('value', type: 'field', matchingRule: 'rule')}
    end

    test "(field:dn:=value)" do
      assert FilterParser.parse("(field:dn:=value)") ==
               {:ok, :eldap.extensibleMatch('value', type: 'field', dnAttributes: true)}
    end

    test "(:dn:rule:=value)" do
      assert FilterParser.parse("(:dn:rule:=value)") ==
               {:ok, :eldap.extensibleMatch('value', dnAttributes: true, matchingRule: 'rule')}
    end
  end

  test "and filter" do
    assert FilterParser.parse("(&(a=1)(b=2)(c=3))") ==
             {:ok,
              :eldap.and([
                :eldap.equalityMatch('a', '1'),
                :eldap.equalityMatch('b', '2'),
                :eldap.equalityMatch('c', '3')
              ])}
  end

  test "or filter" do
    assert FilterParser.parse("(|(a=1)(b=2)(c=3))") ==
             {:ok,
              :eldap.or([
                :eldap.equalityMatch('a', '1'),
                :eldap.equalityMatch('b', '2'),
                :eldap.equalityMatch('c', '3')
              ])}
  end

  test "not filter" do
    assert FilterParser.parse("(!(field=value))") == {:ok, :eldap.not(:eldap.equalityMatch('field', 'value'))}
  end

  test "escape sequences" do
    assert FilterParser.parse("(field=\00\28\29\2a\5c)") == {:ok, :eldap.equalityMatch('field', '\0()*\\')}
  end

  test "invalid filter" do
    assert FilterParser.parse("invalid") == :error
  end
end
