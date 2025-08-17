#!/usr/bin/env bash

EVENTS="$(
    osascript << EOF
      use AppleScript version "2.4"
      use scripting additions
      use framework "Foundation"
      use framework "EventKit"

      -- create start date and end date for occurrences
      set nowDate to current application's NSDate's |date|()
      set todaysDate to current application's NSCalendar's currentCalendar()'s dateBySettingHour:0 minute:0 |second|:0 ofDate:nowDate options:0
      set tomorrowsDate to todaysDate's dateByAddingTimeInterval:1 * days

      -- Initialize an NSDateFormatter to format the date
      set dateFormatter to current application's NSDateFormatter's alloc()'s init()
      dateFormatter's setDateFormat:"yyyy-MM-dd HH:mm:ss"

      -- Convert the NSDate to a formatted string
      set formattedDate to dateFormatter's stringFromDate:tomorrowsDate
      set formattedDateStr to formattedDate as text
      -- log "tomorrowsDate: " & formattedDateStr

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
      set thePred to theEKEventStore's predicateForEventsWithStartDate:nowDate endDate:tomorrowsDate calendars:theCalendars
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

          if minutesRemaining > 0 then
            -- Format remaining time as "Xh Ym"
            set timeRemaining to ""
            if hoursRemaining > 0 then
                set timeRemaining to (hoursRemaining as text) & "h "
            end if
            set timeRemaining to "In " & timeRemaining & (minutesRemaining as text) & "m"

            set eventTitle to (anEvent's title) as text
            set eventDetails to timeRemaining & ";" & eventTitle

            -- Safely get notes and location
            set eventURL to my safeGet(anEvent, "url")
            set eventNotes to my safeGet(anEvent, "notes")
            set eventLocation to my safeGet(anEvent, "location")

            -- log eventURL & eventLocation & eventNotes
            set meetingURL to my findURLInText(eventURL & eventLocation & eventNotes)

            if meetingURL is not "" then
                set eventDetails to eventDetails & ";" & meetingURL
            end if
            set end of eventDetailsList to eventDetails
          end if
      end repeat

      return joinList(eventDetailsList, "__")

      -- Helper to handle property access safely
      on safeGet(anObject, aProperty)
          try
              if anObject's valueForKey:aProperty is missing value then return ""
              return (anObject's valueForKey:aProperty) as text
          on error
              return ""
          end try
      end safeGet

      -- Helper function to find the first URL in the given text
      on findURLInText(theText)
          try
              if theText is "" then return "" -- Avoid processing empty strings

              -- Create an NSDataDetector for URLs
              set theDetector to current application's NSDataDetector's dataDetectorWithTypes:(current application's NSTextCheckingTypeLink) |error|:(missing value)
              set theMatches to theDetector's matchesInString:theText options:0 range:{0, length of theText}

              -- If there are matches, return the URL found in the first match
              if theMatches's |count|() > 0 then
                  set theMatch to theMatches's objectAtIndex:0 -- Get the first match
                  set theURL to theMatch's URL()
                  return theURL's absoluteString() as text
              else
                  return ""
              end if
          on error errMsg
              display dialog "Error finding URL: " & errMsg
              return ""
          end try
      end findURLInText

      -- Helper function to join a list of strings into a single string by given delimiter
      on joinList(theList, delimiter)
          set {TID, text item delimiters} to {text item delimiters, delimiter}
          set joinedString to theList as text
          set text item delimiters to TID
          return joinedString
      end joinList
EOF
)"

echo "$EVENTS"

sketchybar --remove calendar.event.*

if [ -z "$EVENTS" ]; then
    sketchybar --add item calendar.event.0 popup."$NAME" --set calendar.event.0 label="No upcoming events" click_script="sketchybar --set $NAME popup.drawing=off"
    exit 0
fi

COUNTER=0
ARGS=(--set "$NAME" popup.drawing=toggle)
IFS="__" read -ra ETS <<< "$EVENTS"
NEXT_EVENT=""
for EVENT in "${ETS[@]}"; do
    [ -z "$EVENT" ] && continue
    echo $EVENT
    IFS=';' read -ra EDTS <<< "${EVENT## }"
    TIME_REMAINING="${EDTS[0]}"
    EVENT_TITLE="${EDTS[1]}"
    MEETING_URL="${EDTS[2]}"
    LABEL="${TIME_REMAINING} - ${EVENT_TITLE}"
    if [ -z "$NEXT_EVENT" ]; then
        NEXT_EVENT="$LABEL"
    fi
    if [ -n "$MEETING_URL" ]; then
        ARGS+=(--add item calendar.event.$COUNTER popup."$NAME" --set calendar.event.$COUNTER label="$LABEL" click_script="open -n '$MEETING_URL'; sketchybar --set $NAME popup.drawing=off")
    else
        ARGS+=(--add item calendar.event.$COUNTER popup."$NAME" --set calendar.event.$COUNTER label="$LABEL" click_script="sketchybar --set $NAME popup.drawing=off")
    fi
    COUNTER=$(($COUNTER + 1))
done

sketchybar -m "${ARGS[@]}"
sketchybar --set "$NAME" label="$NEXT_EVENT"
