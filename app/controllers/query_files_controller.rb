require 'java'

class QueryFilesController < ApplicationController
  def create
    file = params[:query_file]
    metadata = Java.read file.path
    @query_file = QueryFile.from_upload(file, metadata)
  end
end
