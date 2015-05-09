class KeysController < ApplicationController
  def index
    describe_activity "Listing all keys"
    keys = current_user.analyst.key_materials.order("revoked ASC, created_at DESC")
    @keys_by_type = keys.group_by &:key_type
  end

  def show
    key = current_user.analyst.key_materials.find params[:id]
    describe_activity "Downloaded key #{key.description}"
    respond_to do |format|
      format.pfx { send_data Base64.decode64(key.pkcs12), type: "application/x-pkcs12", filename: key.name("pfx") }
      format.pem { send_data key.pem, type: "application/x-pem-file", filename: key.name("pem") }
      format.html { render text: "Please download key as .pem or .pfx" }
    end
  end

  def destroy
    analyst = current_user.analyst
    key = analyst.key_materials.find params[:id]
    describe_activity "Revoked key #{key.description}"
    analyst.revoke_key key
    redirect_to keys_path, notice: "Key revoked"
  end

  def create
    password = params[:password]
    if password.length < 4 then
      redirect_to keys_path, flash: {error: "The password must be at least 4 characters long"}
    else
      KeyMaterial.create_from_analyst current_user.analyst, password, params[:description], params[:key_type]
      describe_successful_activity "Created key new key with description #{params[:description]}"
      redirect_to keys_path, notice: "New key created"
    end
  end
end
