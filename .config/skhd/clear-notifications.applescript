use AppleScript version "2.4"
use scripting additions

-- 1. Helper for logging
on logDebug(theMessage)
  try
    log theMessage
  end try
end logDebug

-- 2. Recursive Search with Logging
on findSquareButton(theContainer, depth)
  if depth > 10 then
    my logDebug("[Depth Limit] Reached depth 10. Stopping branch.")
    return missing value
  end if

  tell application "System Events"
    try
      -- Get children. If this fails, the container might be empty or restricted.
      set theChildren to UI elements of theContainer
      set childCount to count of theChildren

      if childCount is 0 then
        -- my logDebug("   [Empty] No children in " & (role of theContainer as string))
        return missing value
      end if

      -- Loop through children
      repeat with i from 1 to childCount
        set thisRef to item i of theChildren

        -- Safe property fetching
        set thisRole to "unknown"
        set thisSize to {0, 0}
        set thisDesc to "no desc"

        try
          set thisRole to role of thisRef
          set thisSize to size of thisRef
          set thisDesc to description of thisRef
        on error
          -- my logDebug("   [Error] Could not get props for item " & i)
        end try

        -- Check for button
        if thisRole is "AXButton" then
          set w to item 1 of thisSize
          set h to item 2 of thisSize

          -- Log every button found so we can see if we are missing the target or miscalculating size
          -- my logDebug("   [Check Btn] " & w & "x" & h & " | " & thisDesc)

          if (w = h) and (w ≥ 10) and (w ≤ 50) then
            -- my logDebug(">>> FOUND TARGET: " & w & "x" & h & " " & thisDesc)
            return thisRef
          end if
        end if

        -- Recurse logic
        -- We filter out basic static elements to speed it up, but log if we skip something that looks like a container
        if thisRole is not "AXButton" and thisRole is not "AXStaticText" and thisRole is not "AXImage" then
          -- my logDebug("   [Recurse] Into " & thisRole & " (" & thisDesc & ")")
          set foundBtn to my findSquareButton(thisRef, depth + 1)
          if foundBtn is not missing value then return foundBtn
        end if

      end repeat

    on error errMsg
      my logDebug("!!! ERROR in recursion: " & errMsg)
    end try
  end tell
  return missing value
end findSquareButton

tell application "System Events"
  -- my logDebug("--- STARTING DEBUG RUN ---")

  -- 1. Open Notification Center
  if not (exists window "Notification Center" of application process "NotificationCenter") then
    my logDebug("Opening Notification Center...")
    tell application process "ControlCenter"
      click (first menu bar item of menu bar 1 whose description contains "Clock" or description contains "Date" or description contains "Time")
    end tell

    repeat 20 times
      if exists window "Notification Center" of application process "NotificationCenter" then exit repeat
      delay 0.1
    end repeat
  -- else
  --   my logDebug("Notification Center is already open.")
  end if

  -- 2. Scan
  tell application process "NotificationCenter"
    if exists window "Notification Center" then
      set ncWindow to window "Notification Center"
      my logDebug("Scanning Window: " & (description of ncWindow as string))

      set targetButton to my findSquareButton(ncWindow, 1)

      if targetButton is not missing value then
        -- my logDebug("Clicking Target...")
        click targetButton
      else
        my logDebug("FAILURE: Could not find any 10x10 to 50x50 square button.")
      end if
    else
      my logDebug("ERROR: Window 'Notification Center' not found after waiting.")
    end if
  end tell

  -- 3. Close
  key code 53
  -- my logDebug("--- END ---")
end tell
