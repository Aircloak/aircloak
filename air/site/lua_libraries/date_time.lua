Aircloak.DateTime = {}

local days = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}

function Aircloak.DateTime.hour_range(time)
  return string.format("%02d:00 - %02d:00", time.hour, time.hour + 1)
end

function Aircloak.DateTime.day(time)
  return days[time.wday]
end

function Aircloak.DateTime.date(time)
  return string.format("%04d%02d%02d", time.year, time.month, time.day)
end

function Aircloak.DateTime.week(time)
  return math.floor(time.yday / 7) + 1
end
