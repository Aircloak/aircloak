module VerificationsHelper
  def description_for_event_name name
    event = ClientFileEvent.where(event: name).first
    if event
      event.description
    else
      "Unkown event"
    end
  end
end
