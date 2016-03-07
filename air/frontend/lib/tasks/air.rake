require "./lib/token_generator"

namespace :air do
  desc "Creates CA for REST API if it doesn't exist."
  task :create_api_ca => :environment do
    if File.exist?(KeyMaterial.ca_key_file("api")) and File.exist?(KeyMaterial.ca_cert_file("api"))
      root_cert = File.read(KeyMaterial.ca_cert_file("api"))
      puts "Using existing CA"
    else
      root_key, root_cert = TokenGenerator.generate_root_token("air_web_api", -1)
      File.open(KeyMaterial.ca_key_file("api"), "w") {|file| file.write(root_key)}
      File.open(KeyMaterial.ca_cert_file("api"), "w") {|file| file.write(root_cert)}
      puts "CA created"
    end

    puts "\nAPI certificate:\n#{root_cert}\n"

    puts "Make sure to include this certificate in the list of trusted CA certificates in nginx.\n"
  end
end