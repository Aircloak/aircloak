class OsTagsController < ApplicationController
  def index
    @os_tag = OsTag.new
    @os_tags = OsTag.all
  end

  def new
    @os_tag = OsTag.new
  end

  def create
    @os_tag = OsTag.new(os_tag_params)
    if @os_tag.save
      redirect_to os_tags_path, notice: "Added the os tag, thanks, you are awesome."
    else
      render action: 'new'
    end
  end

  def destroy
    OsTag.find(params[:id]).destroy
    redirect_to os_tags_path, notice: "OsTag removed. Thanks"
  end

private
  def os_tag_params
    params.require(:os_tag).permit(:name, :description)
  end
end
