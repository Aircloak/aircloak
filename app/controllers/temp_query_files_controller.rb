class TempQueryFilesController < ApplicationController
  respond_to :json

  def create
    file = params[:query_file]
    temp = TempQueryFile.create(data: file.read)
    metadata = Java.read file.path
    metadata[:temp_file] = temp.id
    metadata[:indices] = []
    render :json => metadata
  end
end
