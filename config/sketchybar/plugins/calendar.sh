#!/usr/bin/env bash

EVENTS="$(
    osascript << EOF
      use AppleScript version "2.4"
      use scripting additions
      use framework "Foundation"
      use framework "EventKit"

      -- create start date and end date for occurrences
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

      -- find matching events
      set thePred to theEKEventStore's predicateForEventsWithStartDate:todaysDate endDate:tomorrowsDate calendars:theCalendars
      set theEvents to (theEKEventStore's eventsMatchingPredicate:thePred)

      -- sort by date
      set theEvents to theEvents's sortedArrayUsingSelector:"compareStartDateWithEvent:"

      -- Initialize an empty list to hold the event details
      set eventDetailsList to {}

      -- Iterate through the events and calculate time remaining
      repeat with anEvent in theEvents
          set eventStartDate to anEvent's startDate
          set timeInterval to eventStartDate's timeIntervalSinceDate:nowDate
          set hoursRemaining to (timeInterval div 3600)
          set minutesRemaining to (timeInterval mod 3600) div 60

          -- Format remaining time as "Xh Ym"
          set timeRemaining to ""
          if hoursRemaining > 0 then
              set timeRemaining to (hoursRemaining as text) & "h "
          end if
          set timeRemaining to timeRemaining & (minutesRemaining as text) & "m"

          set eventTitle to (anEvent's title) as text
          set end of eventDetailsList to (timeRemaining & " - " & eventTitle)
      end repeat

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
