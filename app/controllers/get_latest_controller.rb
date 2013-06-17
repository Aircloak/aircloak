class GetLatestController < ApplicationController
  def show
    name = params[:id]
    file = ClientFile.where(name: name).first
    if file
      binary = file.client_file_versions.last
      binary.tickle
      send_data binary.data, filename: binary.name, type: "application/octet-stream"
    else
      not_found
    end
  end
end
