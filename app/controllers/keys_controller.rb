class KeysController < ApplicationController
  def index
    @keys = current_user.analyst.key_materials
  end

  def show
    key = current_user.analyst.key_materials.find params[:id]
    send_data Base64.decode64(key.pkcs12), type: "application/x-pkcs12", filename: "#{key.analyst.name}_#{key.id}.pfx"
  end

  def create
    KeyMaterial.create_from_analyst current_user.analyst, params[:password], params[:description]
    redirect_to keys_path, notice: "New key created"
  end
end
