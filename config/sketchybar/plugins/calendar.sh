#!/usr/bin/env bash

EVENTS="$(
    osascript << EOF
  use AppleScript version "2.4"
  use scripting additions
  use framework "Foundation"
  use framework "EventKit"

  -- set listOfCalNames to {"Home - Jase"} -- list of one or more calendar names
  -- set listOfCaTypes to {0} -- list of one or more calendar types: : Local = 0, CalDAV/iCloud = 1, Exchange = 2, Subscription = 3, Birthday = 4
  -- create start date and end date for occurances
  set nowDate to current application's NSDate's |date|()
  set todaysDate to current application's NSCalendar's currentCalendar()'s dateBySettingHour:9 minute:0 |second|:0 ofDate:nowDate options:0
  set tomorrowsDate to todaysDate's dateByAddingTimeInterval:1 * days

  -- create event store and get the OK to access Calendars
  set theEKEventStore to current application's EKEventStore's alloc()'s init()
  theEKEventStore's requestAccessToEntityType:0 completion:(missing value)

  -- check if app has access; this will still occur the first time you OK authorization
  set authorizationStatus to current application's EKEventStore's authorizationStatusForEntityType:0 -- work around enum bug
  if authorizationStatus is not 3 then
    display dialog "Access must be given in System Preferences" & linefeed & "-> Security & Privacy first." buttons {"OK"} default button 1
    tell application "System Preferences"
      activate
      tell pane id "com.apple.preference.security" to reveal anchor "Privacy"
    end tell
    error number -128
  end if

  -- get calendars that can store events
  set theCalendars to theEKEventStore's calendarsForEntityType:0
  -- filter out the one you want
  -- set theNSPredicate to current application's NSPredicate's predicateWithFormat_("title IN %@ AND type IN %@", listOfCalNames, listOfCaTypes)
  set calsToSearch to theCalendars -- 's filteredArrayUsingPredicate:theNSPredicate
  if count of calsToSearch < 1 then error "No such calendar(s)."

  -- find matching events
  set thePred to theEKEventStore's predicateForEventsWithStartDate:todaysDate endDate:tomorrowsDate calendars:calsToSearch
  set theEvents to (theEKEventStore's eventsMatchingPredicate:thePred)
  -- sort by date
  set theEvents to theEvents's sortedArrayUsingSelector:"compareStartDateWithEvent:"
  -- return title property, called summary in AS, for them all
  -- return (theEvents's valueForKey:"title") as list

  -- chatgpt
  set eventDetailsList to {}

  -- Create a date formatter to extract just the time
  set timeFormatter to current application's NSDateFormatter's alloc()'s init()
  timeFormatter's setDateFormat:"HH:mm"

  -- Iterate through the events and extract both time and title
  repeat with anEvent in theEvents
    set eventStartTime to (timeFormatter's stringFromDate:(anEvent's startDate)) as text
    set eventTitle to anEvent's title as text
    set end of eventDetailsList to (eventStartTime & " - " & eventTitle)
  end repeat

  -- Return the list of event details
  return eventDetailsList
EOF
)"

sketchybar --remove calendar.event.*

if [ -z "$EVENTS" ]; then
    sketchybar --add item calendar.event.0 popup."$NAME" --set calendar.event.0 label="No upcoming events" click_script="sketchybar --set $NAME popup.drawing=off"
    exit 0
fi

COUNTER=0
ARGS=(--set "$NAME" popup.drawing=toggle)
IFS=','
read -ra ETS <<< "$EVENTS"
for EVENT in "${ETS[@]}"; do
    ARGS+=(--add item calendar.event.$COUNTER popup."$NAME" --set calendar.event.$COUNTER label="${EVENT## }" click_script="sketchybar --set $NAME popup.drawing=off")
    COUNTER=$(($COUNTER + 1))
done

sketchybar -m "${ARGS[@]}"
sketchybar --set "$NAME" label="${EVENTS%%,*}"
