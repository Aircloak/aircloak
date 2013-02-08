class BackboneController < ApplicationController
  def queries
    @queries = Query.all
    @indices = Index.all
  end
end
