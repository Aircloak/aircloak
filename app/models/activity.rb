class Activity < ActiveRecord::Base
  belongs_to :user

  def success_class
    if success == true
      "success"
    elsif success == false
      "error"
    else
      ""
    end
  end
end
