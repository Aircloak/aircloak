class LookupTable < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :analyst

  validates_presence_of :table_name, :cluster, :analyst
  validate :upload_data_format

  attr_reader :upload_data

  def upload_data=(stream)
    @upload_data = stream.read if stream
  end

private
  def upload_data_format
    return unless errors[:upload_data].empty?
    error = upload_data_error
    if error
      errors.add(:upload_data, error)
    end
  end

  def upload_data_error
    return if self.deleted == true
    return "can't be blank" if @upload_data.to_s == ''
    data = JSON.parse(@upload_data)
    return "json is not valid" unless data.is_a?(Array)
    data.each do |row|
      unless row.is_a?(Array) && row.length == 2 && valid_json_type?(row[0]) && valid_json_type?(row[1])
        return "json is not valid"
      end
    end
    nil
  rescue Exception => e
    "must be in json format"
  end

  def valid_json_type?(value)
    value.kind_of?(Numeric) || value.is_a?(String) || value == true || value == false
  end
end