require 'net/http'
require 'net/https'
require 'uri'

class ProtobufSender
  def self.construct_sock url_string
    url = URI.parse(url_string)
    sock = Net::HTTP.new(url.host, url.port)
    sock.use_ssl = true if url.port == 443
    # FIXME: Get the SSL cert from somewhere
    sock.verify_mode = OpenSSL::SSL::VERIFY_NONE if url.port == 443
    sock
  end

  def self.construct_request url_string, pb
    url = URI.parse(url_string)
    request = Net::HTTP::Post.new(url.path)
    request.content_type = "application/x-protobuf"
    request.body = pb.encode.buf
    request
  end

  def self.post sock, request
    sock.request(request)
  end

  def self.post_to_url url_string, pb
    sock = construct_sock url_string
    request = construct_request url_string, pb
    post sock, request
  end

  def self.send_delete url
    url = URI.parse(url)
    sock = Net::HTTP.new(url.host, url.port)
    sock.use_ssl = true if url.port == 443
    # FIXME: Get the SSL cert from somewhere
    sock.verify_mode = OpenSSL::SSL::VERIFY_NONE if url.port == 443
    request = Net::HTTP::Delete.new(url.path)
    sock.request(request)
  end
end
