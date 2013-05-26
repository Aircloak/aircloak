class ClientFileVersionsController < ApplicationController
  def index
    @client_file_versions = ClientFileVersion.all
  end
end
