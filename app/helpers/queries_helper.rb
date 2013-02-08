module QueriesHelper
  def link_to_add_index(name, f)
    index = Index.new
    id = index.object_id
    fields = f.fields_for(:indices, index, child_index: id) do |builder|
      render("editable_index_fields", f: builder)
    end
    link_to(name, '#', class: "add-new-index btn btn-mini btn-success", data: {id: id, fields: fields.gsub("\n", "")})
  end

  def loaded_form_data
      <<EOF
        data.query_files = #{@query.query_files.to_json(:root => false, :include => :indices, :except => :data)}; 
        data.indices = #{raw Index.all.to_json(:root => false)}; 
        mutator_query = '#{@query.mutator}'; 
        system_query = #{@query.system_query}; 
        identifier_enabled = #{@query.update_query};
EOF

  end
end
