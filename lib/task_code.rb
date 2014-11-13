class TaskCode
  def self.dependencies(code)
    self.new.dependencies(code)
  end

  def dependencies(code)
    dep_names(code).map do |lib_name|
      {name: lib_name, code: available_libs[lib_name]}
    end
  end

private
  def dep_names(code)
    libs =
      code.
          # Match all combinations of word characters and dots up to and
          # including the trailing period. This will extract the namespace
          # prefix of the `Foo.Bar.baz` string (`Foo.Bar.`).
          scan(/(?'namespace'(\w+\.)+)/).
          flatten.
          uniq.
          map {|x| x.gsub(/\.$/, "")}   # Remove trailing dots

    expand(libs).uniq
  end

  def available_libs
    @available_libs ||=
      TaskLibrary.all.inject({}) do |memo, library|
        memo.merge({library.name => library.code})
      end
    @available_libs
  end

  def scanned_libs
    @scanned_libs ||= Set.new
    @scanned_libs
  end

  def expand(libs)
    all_libs = []

    libs.each do |lib|
      parts = lib.split(".")
      prefixes = []
      parts.each do |part|
        prefixes.push(part)
        lib_name = prefixes.join(".")
        if available_libs[lib_name] && !scanned_libs.include?(lib_name)
          scanned_libs << lib_name
          all_libs.concat(dep_names(available_libs[lib_name]))
          all_libs << lib_name
        end
      end
    end
    all_libs
  end
end