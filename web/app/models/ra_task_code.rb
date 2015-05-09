class RaTaskCode < ActiveRecord::Base
  has_many :ra_task_code_clusters
  has_many :ra_library_code_ra_task_codes, dependent: :destroy

  has_many :clusters, through: :ra_task_code_clusters
  has_many :ra_library_codes, through: :ra_library_code_ra_task_codes

  validates_presence_of :prefetch, :code

  def mark_trustworthy
    self.trustworthy = true
    raise "cannot save" unless self.save
    conditionally_mark_answers_resolved
  end

  def conditionally_mark_answers_resolved
    return unless self.trustworthy
    # Each cluster this code belongs to may have unresolved answers which we now can mark resolved as all task
    # codes are trustworthy.  Instead of checking for each task code that all codes are trustworthy, we check
    # that first and then mark the answer as resolved.
    self.clusters.each do |cluster|
      if cluster.ra_task_codes.all? {|task_code| task_code.trustworthy}
        cluster.repeated_answers.where(resolved: false).each {|answer| answer.mark_resolved}
      end
    end
  end

  def self.from_json raw_json
    # first get all libraries and their IDs
    libraries = raw_json["libraries"].to_a.map do |library_json|
      raise "library without a name" if library_json["name"].nil?
      raise "library #{library_json["name"]} without code" if library_json["code"].nil?
      library_code = RaLibraryCode.where(code: library_json["code"]).first
      library_code = RaLibraryCode.create(code: library_json["code"]) unless library_code
      RaLibraryCodeRaTaskCode.create(
        name: library_json["name"],
        ra_library_code: library_code
      )
    end
    library_ids = libraries.map { |connector| connector.ra_library_code.id }

    # check if there is equal task code with the same libraries
    RaTaskCode.where(prefetch: raw_json["prefetch"], code: raw_json["code"]).to_a.each do |task_code|
      if task_code.ra_library_codes.to_a.map{|lc| lc.id} == library_ids
        return task_code
      end
    end

    # if we are here, there is not such a task-code, we need to create one
    return RaTaskCode.create(
      trustworthy: false,
      prefetch: raw_json["prefetch"],
      ra_library_code_ra_task_codes: libraries,
      code: raw_json["code"]
    )
  end
end
