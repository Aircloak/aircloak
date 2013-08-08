class GetLatestController < ApplicationController
  def show
    name = params[:id]
    file = ClientFile.where(local_name: name).first
    if file
      binary = DeploymentGroup.where(identifier: "deployment").first
          .commands.last
          .client_file_versions.where(client_file_id: file.id).last
      if binary
        binary.tickle
        send_data binary.data, filename: binary.name, type: "application/octet-stream"
      else
        not_found
      end
    else
      not_found
    end
  end
end
