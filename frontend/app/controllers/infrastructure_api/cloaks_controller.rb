class InfrastructureApi::CloaksController < ApplicationController
  filter_access_to [:index], require: :anon_read

  def set_layout
    self.class.layout false
  end

  def index
    cloak_lines = []
    Cloak.all.each do |cloak|
      cloak_lines << "#{cloak.name};#{cloak.ip};#{cloak.tpm};#{cloak.good}"
    end
    render text: cloak_lines.join("\n"), content_type: "text/csv"
  end
end
