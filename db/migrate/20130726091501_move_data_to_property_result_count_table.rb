class MoveDataToPropertyResultCountTable < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        ids = {}
        PropertyResult.all.each do |property_result|
          key = key_from_result property_result
          can_be_deleted = false
          id = if ids[key] then
            ids[key]
            can_be_deleted = true
          else
            ids[key] = property_result.id
            property_result.id
          end
          PropertyResultCount.create(property_result_id: id,
                                     count: property_result.count,
                                     created_at: property_result.created_at,
                                     updated_at: property_result.updated_at)
          property_result.destroy if can_be_deleted
        end
        ids = nil
      end
      dir.down do
        PropertyResultCount.all.each do |prc|
          pr = prc.property_result
          if pr.created_at == prc.created_at then
            pr.count = prc.count
            pr.save
          else
            PropertyResult.create(numeric: pr.numeric,
                                  str_value: pr.str_value,
                                  long_value: pr.long_value,
                                  count: prc.count,
                                  property_id: pr.proprety_id,
                                  created_at: prc.created_at,
                                  updated_at: prc.updated_at)
          end
          prc.destroy
        end
      end
    end
  end

  def key_from_result result
    "#{result.property_id}:#{result.str_value}:#{result.numeric.to_s}:#{result.numeric ? result.long_value.to_s : result.str_value}".to_sym
  end
end
