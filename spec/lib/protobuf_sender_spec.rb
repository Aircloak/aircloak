require './lib/protobuf_sender.rb'

describe ProtobufSender do
  it "self.construct_sock should set ssl if https" do
    socket = double
    socket.should_receive(:use_ssl=).with(true)
    socket.stub(:verify_mode=)
    Net::HTTP.should_receive(:new).and_return(socket)
    ProtobufSender.construct_sock "https://www.example.org/"
  end

  it "self.construct_sock should not set ssl if http" do
    socket = double
    socket.should_not_receive(:use_ssl=)
    socket.stub(:verify_mode=)
    Net::HTTP.should_receive(:new).and_return(socket)
    ProtobufSender.construct_sock "http://www.example.org/"
  end

  it "should create a POST request with given protobuf data" do
    data = double
    pb_encode = double
    pb_encode.should_receive(:buf).and_return(data)
    pb = double
    pb.should_receive(:encode).and_return(pb_encode)
    request = double
    request.should_receive(:content_type=).with("application/x-protobuf")
    request.should_receive(:body=).with(data)
    Net::HTTP::Post.should_receive(:new).with("/foo/bar").and_return(request)
    ProtobufSender.construct_request "https://www.example.org/foo/bar", pb
  end

  it "should send the message to destination uri" do
    sock = double
    request = double
    sock.should_receive(:request).with(request)
    ProtobufSender.post(sock, request)
  end

  it "should create socket and post protobuf message to url" do
    data = double
    pb_encode = double
    pb_encode.should_receive(:buf).and_return(data)
    pb = double
    pb.should_receive(:encode).and_return(pb_encode)
    request = double
    request.should_receive(:content_type=).with("application/x-protobuf")
    request.should_receive(:body=).with(data)
    socket = double
    socket.should_receive(:use_ssl=)
    socket.stub(:verify_mode=)
    Net::HTTP.should_receive(:new).and_return(socket)
    socket.should_receive(:request).with(request)
    Net::HTTP::Post.should_receive(:new).with("/baz").and_return(request)
    ProtobufSender.post_to_url "https://www.example.org/baz", pb
  end
end
