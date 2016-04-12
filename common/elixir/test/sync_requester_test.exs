defmodule Air.SyncRequesterTest do
  use ExUnit.Case, async: false

  alias Air.SyncRequester

  test "encoding and decoding" do
    {:ok, _} = SyncRequester.start_link(:client)
    encoded = SyncRequester.encode_request(:client, %{foo: :bar})
    assert {_id, payload} = SyncRequester.decode_request!(encoded)
    assert %{foo: :bar} == payload
  end

  test "request response cycle" do
    {:ok, _} = SyncRequester.start_link(:client)
    encoded_request = SyncRequester.encode_request(:client, :request, :meta)
    {id, _payload} = SyncRequester.decode_request!(encoded_request)
    encoded_response = SyncRequester.encode_response(id, :response)

    assert {:request, :meta, :response} == SyncRequester.decode_response!(:client, encoded_response)
    # pulled response can't be decoded anymore
    assert {nil, nil, :response} == SyncRequester.decode_response!(:client, encoded_response)
  end

  test "unknown response" do
    {:ok, _} = SyncRequester.start_link(:client)
    encoded_response = SyncRequester.encode_response("unknown_id", :response)
    assert {nil, nil, :response} == SyncRequester.decode_response!(:client, encoded_response)
  end

  test "requester isolation" do
    {:ok, _} = SyncRequester.start_link(:client1)
    {:ok, _} = SyncRequester.start_link(:client2)

    encoded_request1 = SyncRequester.encode_request(:client1, :request1, :meta)
    encoded_request2 = SyncRequester.encode_request(:client2, :request2, :meta)

    {id1, _payload} = SyncRequester.decode_request!(encoded_request1)
    encoded_response1 = SyncRequester.encode_response(id1, :response1)

    {id2, _payload} = SyncRequester.decode_request!(encoded_request2)
    encoded_response2 = SyncRequester.encode_response(id2, :response2)

    # request are not known to different requesters
    assert {nil, nil, :response2} == SyncRequester.decode_response!(:client1, encoded_response2)
    assert {nil, nil, :response1} == SyncRequester.decode_response!(:client2, encoded_response1)

    # requests are known to correct requesters
    assert {:request1, :meta, :response1} == SyncRequester.decode_response!(:client1, encoded_response1)
    assert {:request2, :meta, :response2} == SyncRequester.decode_response!(:client2, encoded_response2)
  end

  test "multiple requests and responses out of order" do
    {:ok, _} = SyncRequester.start_link(:client)
    encoded_requests = Enum.map(1..10, &SyncRequester.encode_request(:client, {:request, &1}, :meta))

    # decode in reverse order
    for encoded_request <- Enum.reverse(encoded_requests) do
      {id, {:request, n}} = SyncRequester.decode_request!(encoded_request)
      encoded_response = SyncRequester.encode_response(id, {:response, n})
      decoded = SyncRequester.decode_response!(:client, encoded_response)
      assert {{:request, n}, :meta, {:response, n}} == decoded
    end
  end
end
