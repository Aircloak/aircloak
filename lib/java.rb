require 'javaclass'

class Java
  def self.read path
    jc = JavaClass.load_fs(path)
    {
      :package => jc.full_name,
      :name => jc.simple_name,
      :query_interface => jc.interfaces.any? { |m| m.to_s =~ /RunnableQuery/ },
      :index_ops => jc.references.referenced_methods.any? { |m| m.to_s =~ /addUserToIndex|removeUserFromIndex/ }
    }
  end
end
