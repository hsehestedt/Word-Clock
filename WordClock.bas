Option _Explicit
Option Base 1

' Hannes' Word Clock
' 2022 by Hannes Sehestedt
' Version 1.0.0.7
' May 9, 2022

$ExeIcon:'WordClock.ico'
$VersionInfo:CompanyName=Hannes Sehestedt
$VersionInfo:FILEVERSION#=1,0,0,7
$VersionInfo:ProductName=Hannes Word Clock
$VersionInfo:LegalCopyright=(c) 2022 by Hannes Sehestedt
$Resize:On

ProgramStart:

' In the event that we need to restart the program as a result of an error, we will clear all current
' variables and start over completely clean

Clear

Dim AdjustmentsMade As String ' Flag to indicate that screen resolution adjustments were made automatically to avoid illegal conditions
Dim AllText As String ' Contains a copy of all text that will be displayed on screen. Used to test font sizes.
Dim AM_PM As String ' This flag will be set to either "AM" or "PM"
Dim CharPerLine As Integer ' Number of characters that can fit on a line at a given screen and font size
Dim CharPosition As Integer ' Used to keep track of character positioning while manipulating strings
Dim CurrentDate As String ' Hold the entire date (Month / Day / Year) as a string
Dim CurrentMode As String ' Tracks the current mode. "Y" for fullscreen mode, "N" for windowed mode.
Dim CurrentTime As String ' Hold the entire time (Hours / Minutes / Seconds) as a string
Dim Day As Integer ' Day of the month (1-31) as a number
Dim DayOfWeek As Integer ' Day of the week (1-7) as a number
Dim DayOfWeekString(7) As String ' An array holding each day of the week as an English word
Dim DayString(31) As String ' An array holding each day of the month (1-31) as a string
Dim Decade As Integer ' The numerical value of the last 2 digits of the year
Dim DeskWidth As Long ' The width of the desktop in pixels
Dim DeskHeight As Long ' The height of the desktop in pixels
Dim DigitalHour As String ' Hours converted to a 2 digital string
Dim ff As Integer ' Used to store free file number
Dim FileLine As String ' Line read from a file
Dim Font As Long ' Handle to a font
Dim FontHeight As Integer ' Height of a font in windowed mode
Dim FontPath As String ' The name of the font, with path, used in windowed mode
Dim FontSize As Integer ' The fontsize used in windowed mode
Dim FontSizeToTest As Integer ' In the routine that tests fit of a font on the screen, this holds the current font size to be tested
Dim FontTooLarge As String ' Flag to indicate when a font is too large to properly display all text
Dim FontWidth As Integer ' Width of a font in windowed mode
Dim FullscreenFontHeight As Integer
Dim FullscreenFontPath As String ' The name of the font, with path, used in fullscreen mode
Dim FullscreenFontSize As Integer ' The fontsize used in fullscreen mode
Dim FullscreenFontWidth As Integer ' Width of a font in fullscreen mode
Dim handle As Long ' Stores a handle to the screen
Dim HandleErrors As String ' If set to "Y" then error handling is enabled, otherwise it is disabled
Dim High As Integer ' The height of a font undergoing testing to see if it allows text to properly fit on screen
Dim Horizontal As Integer ' The horizontal resolution used in windowed mode
Dim Hour As Integer ' Numerical value holding the current hour (0-23)
Dim HourString(12) As String ' The current hour as an English word. Since we use AM / PM this holds only one through twelve.
Dim KeyPress As String ' Used to sore keystrokes from Inkey$
Dim LeapYear As Integer ' To to indicate if current year is a leap year. 1 = Leap Year, 0 = No Leap Year
Dim LineCount As Integer ' In the routine to set font sizes, this keeps track of how many lines of text a given font size will occupy
Dim MaxLines As Integer ' The maximum number of lines of text that will fit on the screen at a given screen and font size
Dim Minute As Integer ' The current minute as a numeral from 0 to 59
Dim MinuteString(59) As String ' An array hold minutes as English words from one to fifty-nine
Dim Month As Integer ' The current month as a number from 1 to 12
Dim MonthString(12) As String ' The current month as an English word (January, February, ... , November, December).
Dim MonthTable(12) As Integer ' A table with an offset for each month used to calculate what day of the week it is (Monday, Tuesday, etc).
Dim oldimage As Long ' holds the handle of a screen that is about to be removed from memory
Dim OldSecond As Integer ' A variable that is used to determine if the seconds have changed from the last time we checked
Dim ProgramVersion As String ' Holds the current program version
Dim Result1 As Integer ' A temporary variable
Dim Result2 As Integer ' A temporary variable
Dim Result3 As Integer ' A temporary variable
Dim Second As Integer ' The current seconds as a number (0-59)
Dim SecondString(59) As String ' The current seconds as an English word from one through fifty-nine
Dim ShowDigitalTime As String ' Set to "Y" to show digital time on the last line of the screen
Dim StartFullscreen As String ' Set to "Y" to start the program in fullscreen mode, "N" to start in windowed mode
Dim Temp As Integer ' A temporary variable
Dim Temp2 As Integer ' A temporary variable
Dim TempString As String ' A temporary string of characters, used mainly in string manipulation routines
Dim Vertical As Integer ' Vertical resolution used in windowed mode
Dim Wide As Integer ' The width of a font undergoing testing to see if it allows text to properly fit on screen
Dim x As Integer ' General purpose counter used in FOR...NEXT loops
Dim Year As Integer ' Stores the current year

ProgramVersion$ = "1.0.0.7"
_Title "Hannes' Word Clock " + ProgramVersion$

' Default values used for entries not available from a .ini file and initialization of other variables
' NOTE: Font size is determined dynamically. The FontSize and FullscreenFontSize variables are set here simply to serve
' as starting points where the dynamic font adjustment routine can start.

DeskWidth = _DesktopWidth
DeskHeight = _DesktopHeight
FontPath$ = Environ$("SYSTEMROOT") + "\fonts\lucon.ttf"
FullscreenFontPath$ = Environ$("SYSTEMROOT") + "\fonts\lucon.ttf"
FontSize = 14
FullscreenFontSize = 35
Horizontal = 800
Vertical = 600
StartFullscreen$ = "Y"
ShowDigitalTime$ = "Y"
AllText$ = "" ' Initialize the string to an empty state
HandleErrors$ = "Y"


' If a .ini file exists, open it and parse it. Values found in the .ini will override the defaults
' defined above.

If _FileExists("WordClock.ini") Then

    ff = FreeFile
    Open "WordClock.ini" For Input As #ff

    Do Until EOF(ff)
        Line Input #ff, FileLine$

        ' If line starts with a colon (:), it is a comment. Ignore it.

        If Left$(FileLine$, 1) = ":" Then _Continue

        ' If line starts with "FONT:" then we are reading in the name of the font to be used. This is NOT case sensitive.

        If UCase$(Left$(FileLine$, 5)) = "FONT:" Then
            TempString$ = Environ$("SYSTEMROOT") + "\fonts\" + Right$(FileLine$, (Len(FileLine$) - 5))
            If _FileExists(TempString$) Then FontPath$ = TempString$
        End If

        ' If line starts with "FULLSCREENFONT:" then we are reading in the name of the font to be used
        ' in fullscreen mode. This is NOT case sensitive.

        If UCase$(Left$(FileLine$, 15)) = "FULLSCREENFONT:" Then
            TempString$ = Environ$("SYSTEMROOT") + "\fonts\" + Right$(FileLine$, (Len(FileLine$) - 15))
            If _FileExists(TempString$) Then FullscreenFontPath$ = TempString$
        End If

        ' If line starts with "WINDOWHORIZONTAL:", use value to set the horizontal window size.

        If UCase$(Left$(FileLine$, 17)) = "WINDOWHORIZONTAL:" Then
            Horizontal = Val(Right$(FileLine$, (Len(FileLine$) - 17)))
        End If

        ' If line starts with "WINDOWVERTICAL:", use value to set the vertical window size.

        If UCase$(Left$(FileLine$, 15)) = "WINDOWVERTICAL:" Then
            Vertical = Val(Right$(FileLine$, (Len(FileLine$) - 15)))
        End If

        ' If line starts with "STARTFULLSCREEN:", read value that will determine if the program is to be
        ' started fullscreen or in a window.

        If UCase$(Left$(FileLine$, 16)) = "STARTFULLSCREEN:" Then
            StartFullscreen$ = UCase$(Right$(FileLine$, (Len(FileLine$) - 16)))
        End If

        ' If line starts with "SHOWDIGITALTIME:", read value that will determine if digital time is
        ' to be displayed on the last line of the screen.

        If UCase$(Left$(FileLine$, 16)) = "SHOWDIGITALTIME:" Then
            ShowDigitalTime$ = UCase$(Right$(FileLine$, (Len(FileLine$) - 16)))
        End If


        ' If line starts with "HANDLEERRORS:", read value that will determine if error handling is to be
        ' enabled or not.

        If UCase$(Left$(FileLine$, 13)) = "HANDLEERRORS:" Then
            HandleErrors$ = UCase$(Right$(FileLine$, (Len(FileLine$) - 13)))
        End If

    Loop

    Close #ff

    ' If HandleErrors$ is set to "Y" then enable error handling

    If HandleErrors$ = "Y" Then
        On Error GoTo HandleErrors
    End If

End If

' Setup screen for either fullscreen or windowed mode

If StartFullscreen$ = "Y" Then
    CurrentMode$ = "FULLSCREEN"
    handle& = _NewImage(_DesktopWidth, _DesktopHeight, 256)
    Screen handle&
    Sleep 1
    _FullScreen
    GoSub FindLargestFontSize
    Font& = _LoadFont(FullscreenFontPath$, FullscreenFontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(_DesktopHeight / High)
    FullscreenFontWidth = _FontWidth
    FullscreenFontHeight = _FontHeight
Else
    CurrentMode = "WINDOWED"
    handle& = _NewImage(Horizontal, Vertical, 256)
    Screen handle&
    Sleep 1
    _FullScreen _Off
    GoSub FindLargestFontSize
    Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(Vertical / High)
    FontWidth = _FontWidth
    FontHeight = _FontHeight
End If

' Read the spelled out version of various elements into arrays. This will save time later so that we don't have to constantly
' parse this over and over in out main program loop.

Restore DayOfWeek
For x = 1 To 7
    Read DayOfWeekString$(x)
Next x

Restore Day
For x = 1 To 31
    Read DayString$(x)
Next x

Restore Month
For x = 1 To 12
    Read MonthString$(x)
Next x

Restore Hour
For x = 1 To 12
    Read HourString$(x)
Next x

Restore Minute
For x = 1 To 59
    Read MinuteString$(x)
Next x

Restore Second
For x = 1 To 59
    Read SecondString$(x)
Next x

Restore MonthTable
For x = 1 To 12
    Read MonthTable(x)
Next x

Cls

' Clear the keyboard buffer before we enter the main program loop.

Do While InKey$ <> ""
Loop

' This is the main loop that retrieves the date and time, breaks it down into individual components, and then
' displays the time and date in words.

Do
    _Limit 60 ' Limit the number of times that we perform this loop to a maximum of 60 iterations per second

    If _Resize Then

        'If we are NOT running fullscreen, then resize the screen appropriately.

        If (_ResizeWidth <> _DesktopWidth) And (_ResizeHeight <> _DesktopHeight) Then
            Horizontal = _ResizeWidth: Vertical = _ResizeHeight
            oldimage& = handle&
            handle& = _NewImage(Horizontal, Vertical, 256)
            Screen handle&
            _FullScreen _Off
            _FreeImage oldimage&
            Sleep 1
            GoSub FindLargestFontSize
            Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
            _Font Font&
            High = _FontHeight
            Wide = _FontWidth
            MaxLines = Int(Vertical / High)
            FontWidth = _FontWidth
            FontHeight = _FontHeight
        End If
    End If

    ' The lines below check for any keypresses. If a hotkey is pressed, then we take the appropriate action.
    ' Pressing any other key will exit the program. This is most useful when the program is being used as a
    ' screensaver.

    KeyPress$ = InKey$

    Select Case KeyPress$
        Case ""
            Exit Select
        Case "D", "d"
            If ShowDigitalTime$ = "Y" Then
                ShowDigitalTime$ = "N"
            Else
                ShowDigitalTime$ = "Y"
            End If
        Case "F", "f"
            If CurrentMode = "WINDOWED" Then
                CurrentMode = "FULLSCREEN"
                oldimage& = handle&
                handle& = _NewImage(_DesktopWidth, _DesktopHeight, 256)
                Screen handle&
                Sleep 1
                _FullScreen , _Smooth
                _FreeImage oldimage&
                GoSub FindLargestFontSize
                Font& = _LoadFont(FullscreenFontPath$, FullscreenFontSize, "MONOSPACE")
                _Font Font&
                High = _FontHeight
                Wide = _FontWidth
                MaxLines = Int(_DesktopHeight / High)
                FullscreenFontWidth = _FontWidth
                FullscreenFontHeight = _FontHeight
            ElseIf CurrentMode = "FULLSCREEN" Then
                CurrentMode = "WINDOWED"
                oldimage& = handle&
                handle& = _NewImage(Horizontal, Vertical, 256)
                Screen handle&
                Sleep 1
                _FullScreen _Off
                _FreeImage oldimage&
                GoSub FindLargestFontSize
                Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
                _Font Font&
                High = _FontHeight
                Wide = _FontWidth
                MaxLines = Int(Vertical / High)
                FontWidth = _FontWidth
                FontHeight = _FontHeight
            End If
            Sleep 1
        Case "S", "s"
            GoSub DisplayStats
        Case "H", "h"
            GoSub Help
        Case Else
            System
    End Select

    ' Begin breaking down the time and date into individual components

    CurrentDate$ = Date$
    CurrentTime$ = Time$
    Month = Val(Left$(CurrentDate$, 2))
    Day = Val(Mid$(CurrentDate$, 4, 3))
    Year = Val(Right$(CurrentDate$, 4))
    Decade = Val(Right$(CurrentDate$, 2))
    Hour = Val(Left$(CurrentTime$, 2))
    Minute = Val(Mid$(CurrentTime$, 4, 2))
    Second = Val(Right$(CurrentTime$, 2))

    ' At the end of the loop that displays the time on the screen, we set OldSecond to the current seconds. When we reach
    ' this point again, if the current seconds are still the same, we skip the display process since there are no changes.
    ' If the seconds have changed, then proceed with updating the display.

    If (OldSecond = Second) Then GoTo DisplayFinished

    ' Calculate the day of the week
    ' IMPORTANT: The calculations below are valid through 2099.

    ' Step 1: Add the day of the month and the number from the month table. We will read the values from the month table.

    Temp = Day + MonthTable(Month)

    ' Step 2: If the number calculated above is greater than 6, then subtract the highest multiple of 7 from this number.

    If Temp > 6 Then
        Temp2 = Int(Temp / 7)
        Temp = Temp - (Temp2 * 7)
    End If

    Result1 = Temp

    ' Step 3: From the last two digits of the year, subtract the year that has the highest multiple of 28.

    Temp = Decade

    If Decade > 27 Then
        Temp2 = Int(Temp / 28)
        Temp = Decade - (Temp2 * 28)
    End If

    Result2 = Temp

    ' Step 4: Take the last 2 digits of the year, divide by 4, and drop anything after the decimal point. Add that value to Result2.

    Temp = 0

    If Decade > 3 Then
        Temp = Int(Decade / 4)
    End If

    Result3 = Result2 + Temp

    ' Step 5: If the month is Jan or Feb AND the year is a leap year, subtract 1 from Result3.

    If Month < 3 Then
        If (Year / 4) = (Int(Year / 4)) Then
            LeapYear = 1
        Else
            LeapYear = 0
        End If
        Result3 = Result3 - LeapYear
    End If

    ' Step 6: Add Result1 and Result3. Subtract the highest multiple of 7. The result will be 0-6 with 0 being Sat, and 6 being Fri.

    Result3 = Result3 + Result1

    If Result3 > 6 Then
        Temp = Int(Result3 / 7)
        Result3 = Result3 - (Temp * 7)
    End If

    ' To make handling easier, we will add 1 to result so that the day of the week will now be a number from 1 to 7. The
    ' end result is that Sat = 1, Fri = 7.

    DayOfWeek = Result3 + 1

    ' End calculation of the day of the week.

    ' Set the default color of items printed to the screen to grey on black. Current values will be highlighted.
    ' Currently, this means white text on a red background, but we intend to allow customization later in a future
    ' version of the program. For now, we are simply hard coding it.

    Locate 1, 1
    Color 8, 0

    ' Print all days of the week

    For x = 1 To 7
        If x = DayOfWeek Then
            Color 15, 4: Print DayOfWeekString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print DayOfWeekString$(x);: GoSub LeftJustify
        End If
    Next x

    ' Always print the word "the" in the highlight color

    Color 15, 4: Print "the";: Color 8, 0: GoSub LeftJustify

    ' Print the day of the month

    For x = 1 To 31
        If x = Day Then
            Color 15, 4: Print DayString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print DayString$(x);: GoSub LeftJustify
        End If
    Next x

    ' Always print the word "of" in the highlight color

    Color 15, 4: Print "of";: Color 8, 0: GoSub LeftJustify

    ' Print the month

    For x = 1 To 12
        If x = Month Then
            Color 15, 4: Print MonthString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print MonthString$(x);: GoSub LeftJustify
        End If
    Next x

    ' Always print a comma (,) in the highlight color

    Color 15, 4: Print ",";: Color 8, 0: GoSub LeftJustify

    ' Print the hour. Hours are numbered from 0 to 23. Since we are using AM and PM we need to manipulate the hours a little bit
    ' and set an AM / PM flag.

    ' Set an AM / PM Flag. AM_PM$ will be set to either "AM" or "PM".

    Select Case Hour
        Case 0 To 11
            AM_PM$ = "AM"
        Case Else
            AM_PM$ = "PM"
    End Select

    ' Convert 24 hour time to AM / PM (12 hour) format

    Select Case Hour
        Case 0
            Hour = Hour + 12
            Exit Select
        Case 13 To 23
            Hour = Hour - 12
            Exit Select
    End Select

    For x = 1 To 12
        If x = Hour Then
            Color 15, 4: Print HourString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print HourString$(x);: GoSub LeftJustify
        End If
    Next x

    ' If minutes are equal to zero, highlight the word "o'clock".

    If (Minute = 0) Then
        Color 15, 4: Print "o'clock";: Color 8, 0: GoSub LeftJustify
    Else
        Print "o'clock";: GoSub LeftJustify
    End If

    ' Print the minute. Minutes are numbered from 0 to 59. If seconds are 0, then we highlight the word "precisely",
    ' otherwise we highlight the word "and" and the appropriate second following the minutes.

    For x = 1 To 59
        If x = Minute Then
            Color 15, 4: Print MinuteString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print MinuteString$(x);: GoSub LeftJustify
        End If
    Next x

    ' Print the AM and PM indicators.

    Select Case AM_PM$
        Case "AM"
            Color 15, 4: Print "AM";: Color 8, 0: GoSub LeftJustify: Print "PM";: GoSub LeftJustify
        Case "PM"
            Print "AM";: GoSub LeftJustify: Color 15, 4: Print "PM";: Color 8, 0: GoSub LeftJustify
    End Select

    ' If seconds are 0, then highlight the word "precisely", otherwise, highlight the word "and".

    Select Case Second
        Case 0
            Print "and";: GoSub LeftJustify
            Color 15, 4: Print "precisely";: Color 8, 0: GoSub LeftJustify
        Case Else
            Color 15, 4: Print "and";: Color 8, 0: GoSub LeftJustify
            Print "precisely";: GoSub LeftJustify
    End Select

    ' Print the second. Seconds are numbered from 0 to 59.

    For x = 1 To 59
        If Second = x Then
            Color 15, 4: Print SecondString$(x);: Color 8, 0: GoSub LeftJustify
        Else
            Print SecondString$(x);: GoSub LeftJustify
        End If
    Next x

    ' Highlight the word "second" if Second = 1, otherwise highlight "seconds" if Second > 1.

    Select Case Second
        Case 0
            Print "second";: GoSub LeftJustify: Print "seconds";
        Case 1
            Color 15, 4: Print "second";: Color 8, 0: GoSub LeftJustify: Print "seconds";
        Case Else
            Print "second";: GoSub LeftJustify: Color 15, 4: Print "seconds";: Color 8, 0
    End Select

    OldSecond = Second

    DisplayFinished:

    If CurrentMode$ = "FULLSCREEN" Then
        CharPerLine = Int(_DesktopWidth / Wide)
    Else
        CharPerLine = Int(Horizontal / Wide)
    End If

    Locate MaxLines, (CharPerLine / 2) - 5

    If ShowDigitalTime$ = "N" Then
        Print "           ";
        GoTo EndShowDigitalTime
    End If

    Select Case Hour
        Case 0
            DigitalHour$ = "12"
        Case 1 To 9
            DigitalHour$ = "0" + LTrim$(Str$(Hour))
        Case 10 To 12
            DigitalHour$ = LTrim$(Str$(Hour))
        Case 13 To 21
            DigitalHour$ = "0" + LTrim$(Str$(Hour - 12))
        Case 22, 23
            DigitalHour$ = LTrim$(Str$(Hour - 12))
    End Select

    Color 0, 2
    Print DigitalHour$; ":"; Mid$(CurrentTime$, 4, 2); ":"; Right$(CurrentTime$, 2);
    Color 8, 0
    Print " ";: Color 0, 2: Print AM_PM$;
    Color 8, 0

    EndShowDigitalTime:

Loop


' SUBROUTINES


LeftJustify:

' This routine ensures that spaces are not printed in the first column of a line. This has the effect
' of ensuring that all lines are left justified.

If Pos(0) > 1 Then Print " ";

Return


Help:

' Display help and usage instructions for the program.

_FullScreen _Off
Screen 0
Width 120, 30
Print "This program was inspired by a screen saver authored by Simon Heys many years ago and called Word Clock. To use the"
Print "program effectivly, you should know how about the following two items:"
Print
Print "1) The WordClock.ini file"
Print "2) Program hotkeys"
Print "3) Program defaults"
Print
Input "Press <ENTER> to continue...", Temp
Cls
Print "The WordClock.ini File"
Print "-------------------------"
Print
Print "Entries in the WordClock.ini file are not case sensitive. You can use uppercase, lowercase, or mixedcase.  Any line"
Print "that starts with a colon (:) as the first character is considered a comment and will be ignored by the program. The"
Print "entries that the program recognizes are described below. Please note that the .ini file should be placed in the same"
Print "location as the program itself. Follow each entry with a colon and a value. See examples below."
Print
Print ": This is a comment       -  Comments start with a "; Chr$(34); ":"; Chr$(34); " as the first character and are ignored by the program."
Print ": Windowed mode entries"
Print "Font:lucon.ttf            -  The font to be used in windowed mode. Font name only, no path."
Print "Fontsize:14               -  * Size of font used in windowed mode. Font name only, no path."
Print "WindowHorizontal:800      -  Horizontal resolution (width) used in windowed mode."
Print "WindowVertical:600        -  Vertical resolution (height) used in windowed mode."
Print ": Full screen mode entries"
Print ""
Print "FullscreenFont:lucon.ttf  -  The font to be used in fullscreen mode."
Print "FullscreenFontSize:32     -  * Size of font used in fullscreen mode."
Print "StartFullscreen:Y         -  Specify Y to start in fullscreen mode, N to start windowed."
Print ": Other entries"
Print "ShowDigitalTime:Y         -  Display the digital time on the last line of the screen."
Print "HandleErrors:Y            -  Enables error handling routies. Disable if you need to see original QB64 error message(s)."
Print
Print "* Note that the program now dynamically adjust the font size so the font size entries are obsolete. However, they may"
Print "  still serve one purpose: When the program begins performing dynamic adjustment of the font size, it will use the"
Print "  specified fontsize as a starting point. If you have an especially large monitor and a very large font is needed,"
Print "  specifying a font that is close to right size may enable the dynamic adjustment to determine a solution more quickly."
Print
Input "Press <ENTER> to continue...", Temp
Cls
Print "Program Hotkeys"
Print "---------------"
Print
Print "Hotkeys (Note case sensitivity)"
Print
Print "D or d : Toggles between displaying / not displaying digital time at the bottom of the screen."
Print "F or f : Toggle in and out of fullscreen mode."
Print "H or h : Displays help for the program."
Print "S or s : Display statistics / current values of options."
Print
Print "Any other Key will exit the program."
Print
Print "Please note that the values shown by the Statistics hotkey are the current values in use in the program. As an example,"
Print "if you have changed the screen size in the windowed mode, the windowed mode width and height will reflect the current"
Print "settings, not the program default settings or the settings you provide in the WordClock.ini file."
Print
Input "Press <ENTER> to continue...", Temp
Cls
Print "Program Defaults"
Print "----------------"
Print
Print "If no WordClock.ini file is present, or for any missing items in that file, the following defaults are used:"
Print
Print "Font:lucon.ttf"
Print "WindowHorizontal:800"
Print "WindowVertical:600"
Print "FullscreenFont:lucon.ttf"
Print "StartFullscreen:Y"
Print "ShowDigitalTime:Y"
Print
Input "Press <ENTER> to continue...", Temp
Cls

' Set the screen back to the mode it was in before we called help.

If CurrentMode$ = "FULLSCREEN" Then
    handle& = _NewImage(_DesktopWidth, _DesktopHeight, 256)
    Screen handle&
    Sleep 1
    _FullScreen
    Font& = _LoadFont(FullscreenFontPath$, FullscreenFontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(_DesktopHeight / High)
    FullscreenFontWidth = _FontWidth
    FullscreenFontHeight = _FontHeight
End If

If CurrentMode = "WINDOWED" Then
    handle& = _NewImage(Horizontal, Vertical, 256)
    Screen handle&
    Sleep 1
    _FullScreen _Off
    Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(Vertical / High)
    FontWidth = _FontWidth
    FontHeight = _FontHeight
End If

Return


DisplayStats:

' Display current settings being used in the program. This includes fullscreen / windowed height / width, etc.
' Once a user has settings dialed in where they want, this is a helpful way of getting all the values needed
' to plug into the .ini file.


Screen 0
Width 120, 30

_FullScreen Off
_FreeImage handle&

Cls
Print "The values shown below are current values, not program default settings or the settings from the WordClock.ini file."
Print "Note that a value may shows up as "; Chr$(34); "0"; Chr$(34); ", if that mode has not been used yet. For example, windowed font width and height"
Print "may show as zero until you use windowed mode for the first time. Use the WordClock.ini file to alter the default"
Print "behavior of the program."
Print
Print "     Windowed Mode Options"
Print "-------------------------------"
Print "Font used in windowed mode: "; FontPath
Print "Font size in windowed mode:"; FontSize
Print "Windowed screen font height:"; FontHeight
Print "Windowed screen font width:"; FontWidth
Print "Windowed mode width:"; Horizontal
Print "Windowed mode height:"; Vertical
Print
Print "   Full Screen Mode Options"
Print "-------------------------------"
Print "Font used in fullscreen mode: "; FullscreenFontPath
Print "Font size in fullscreen mode:"; FullscreenFontSize
Print "Full screen font height:"; FullscreenFontHeight
Print "Full screen font width:"; FullscreenFontWidth
Print "Fullscreen width (cannot be changed):"; DeskWidth
Print "Fullscreen height (cannot be changed):"; DeskHeight
Print
Print "         Other Options"
Print "-------------------------------"
Print "Show digital time at bottom of screen:"; ShowDigitalTime$
Print "Error handling routines:"; HandleErrors$
Print
Input "Press <ENTER> to continue...", Temp
Cls

' Set the screen back to the mode it was in before we called help.

If CurrentMode$ = "FULLSCREEN" Then
    handle& = _NewImage(_DesktopWidth, _DesktopHeight, 256)
    Screen handle&
    Sleep 1
    _FullScreen
    Font& = _LoadFont(FullscreenFontPath$, FullscreenFontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(_DesktopHeight / High)
    FullscreenFontWidth = _FontWidth
    FullscreenFontHeight = _FontHeight
End If

If CurrentMode = "WINDOWED" Then
    handle& = _NewImage(Horizontal, Vertical, 256)
    Screen handle&
    Sleep 1
    _FullScreen _Off
    Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
    _Font Font&
    High = _FontHeight
    Wide = _FontWidth
    MaxLines = Int(Vertical / High)
    FontWidth = _FontWidth
    FontHeight = _FontHeight
End If

Return


FindLargestFontSize:

GoSub ValidateScreen

' This subroutine will determine what the largest font size is that all allow all text to be displayed on the screen.
' This subroutine will in turn call the subroutine "FontSizeTest".

' We will begin our testing with the currently set font size

Select Case CurrentMode$
    Case "FULLSCREEN"
        FontSizeToTest = FullscreenFontSize
    Case "WINDOWED"
        FontSizeToTest = FontSize
End Select

' Start testing font sizes to see if they are too large. We begin by testing one size larger than the initial font size
' specified. Once we encounter a failure then we back off the size until it passes. Once we have a pass we then have the
' largest font size that works. Before testing a font size, load all the text to be displayed into AllText$.

Do
    FontSizeToTest = FontSizeToTest + 1
    Restore AllText
    AllText$ = ""

    Do
        Read TempString$
        If TempString$ = "EOF" Then Exit Do
        AllText$ = AllText$ + TempString$
    Loop

    GoSub FontSizeTest
Loop Until FontTooLarge$ = "Y"

Do
    FontSizeToTest = FontSizeToTest - 1

    Restore AllText
    AllText$ = ""
    Do
        Read TempString$
        If TempString$ = "EOF" Then Exit Do
        AllText$ = AllText$ + TempString$
    Loop

    GoSub FontSizeTest
Loop Until FontTooLarge$ = "N"

' We reach this point when the largest font size has been determined.
' Assign this font size to either the FontSize or FullscreenFontSize variable.

Select Case CurrentMode$
    Case "FULLSCREEN"
        FullscreenFontSize = FontSizeToTest
    Case "WINDOWED"
        FontSize = FontSizeToTest
End Select

Return


FontSizeTest:

Select Case CurrentMode$
    Case "FULLSCREEN"

        Font& = _LoadFont(FullscreenFontPath$, FontSizeToTest, "MONOSPACE")
        _Font Font&
        High = _FontHeight
        Wide = _FontWidth
        CharPerLine = Int(_DesktopWidth / Wide)
        MaxLines = Int(_DesktopHeight / High)

        ' We are reserving the last line for use by the Digital Clock option, so we are subtracting 1 line from MaxLines

        MaxLines = MaxLines - 1

    Case "WINDOWED"

        Font& = _LoadFont(FontPath$, FontSizeToTest, "MONOSPACE")
        _Font Font&
        High = _FontHeight
        Wide = _FontWidth
        CharPerLine = Int(Horizontal / Wide)
        MaxLines = Int(Vertical / High)

        ' We are reserving the last line for use by the Digital Clock option, so we are subtracting 1 line from MaxLines

        MaxLines = MaxLines - 1

End Select

LineCount = 0 ' Set an initial value before entering the loop

Do

    ' If AllText$ has a space as the first character, remove it. Since we always left justify the output,
    ' a space at the beginning of a line is dropped and should not count toward the character limit for a line.

    AllText$ = LTrim$(AllText$)

    ' If AllText$ has zero length after trimming, then font size is not too large and there is nothing
    ' more to be done so we exit from this test.

    If Len(AllText$) = 0 Then
        FontTooLarge$ = "N"
        Exit Do
    End If

    ' If the length of the AllText$ is greater than the number of characters that we can fit on a line, then
    ' read the number of characters a line can hold plus one more. By doing this, we can check the last
    ' character to see if it is a space. If it is a space then the last character on the line is the
    ' last character of a word. However, if that character is a letter, then we are cutting off a word
    ' and need to determine where that word started.

    If Len(AllText$) > CharPerLine Then
        TempString$ = Left$(AllText$, CharPerLine + 1)

        If Right$(TempString$, 1) = " " Then
            AllText$ = LTrim$(Right$(AllText$, (Len(AllText$) - CharPerLine)))
        Else
            CharPosition = _InStrRev(TempString$, " ")
            TempString$ = Left$(TempString$, CharPosition - 1)
            AllText$ = Right$(AllText$, Len(AllText$) - Len(TempString$))
        End If

        LineCount = LineCount + 1

        If LineCount > MaxLines Then
            FontTooLarge$ = "Y"
            Exit Do
        Else
            FontTooLarge$ = "N"
            _Continue
        End If

        _Continue
    End If

    ' If the number of characters left in AllText$ is <= to the max length of line, then
    ' we can increment the LineCount and exit this loop.

    If Len(AllText$) <= CharPerLine Then

        LineCount = LineCount + 1

        If LineCount > MaxLines Then
            FontTooLarge$ = "Y"
            Exit Do
        Else
            FontTooLarge$ = "N"
        End If
        Exit Do
    End If

Loop

Return


' Check for invalid screen sizes (smaller than 200 x 200)

ValidateScreen:

AdjustmentsMade$ = "N" ' Set initial value

If Horizontal >= _DesktopWidth Then
    Horizontal = _DesktopWidth - 1
    AdjustmentsMade$ = "Y"
End If

If Vertical >= _DesktopHeight Then
    Vertical = _DesktopHeight - 1
    AdjustmentsMade$ = "Y"
End If

If Horizontal < 200 Then
    Horizontal = 200
    AdjustmentsMade$ = "Y"
End If

If Vertical < 200 Then
    Vertical = 200
    AdjustmentsMade$ = "Y"
End If

If AdjustmentsMade = "N" Then GoTo EndAdjustments

AdjustScreenSize:

oldimage& = handle&
handle& = _NewImage(Horizontal, Vertical, 256)
Screen handle&
_FullScreen _Off
_FreeImage oldimage&
Sleep 1

GoSub FindLargestFontSize
Font& = _LoadFont(FontPath$, FontSize, "MONOSPACE")
_Font Font&
High = _FontHeight
Wide = _FontWidth
MaxLines = Int(Vertical / High)
FontWidth = _FontWidth
FontHeight = _FontHeight

EndAdjustments:

Return


''''''''''''''''''''''
' End of subroutines '
''''''''''''''''''''''


''''''''''''''''''
' Error Handling '
''''''''''''''''''

HandleErrors:

' At the time of this writing, there are no known errors that need to be handled.
'
' Please note that error handling can be disabled by adding an entry to the WordClock.ini file like this
''
Resume ProgramStart


' End of main program

End


' DATA section


DayOfWeek:
Data "Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"

Day:
Data "first","second","third","fourth","fifth","sixth","seventh","eighth","ninth","tenth","eleventh","twelfth","thirteenth"
Data "fourteenth","fifteenth","sixteenth","seventeenth","eighteenth","nineteenth","twentieth","twenty-first","twenty-second"
Data "twenty-third","twenty-fourth","twenty-fifth","twenty-sixth","twenty-seventh","twenty-eighth","twenty-ninth","thirtieth","thirty-first"

Month:
Data "January","February","March","April","May","June","July","August","September","October","November","December"

Hour:
Data "one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve"

Minute:
Data "oh-one","oh-two","oh-three","oh-four","oh-five","oh-six","oh-seven","oh-eight","oh-nine","ten","eleven","twelve","thirteen"
Data "fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","twenty-one","twenty-two","twenty-three","twenty-four"
Data "twenty-five","twenty-six","twenty-seven","twenty-eight","twenty-nine","thirty","thirty-one","thirty-two","thirty-three"
Data "thirty-four","thirty-five","thirty-six","thirty-seven","thirty-eight","thirty-nine","forty","forty-one","forty-two","forty-three"
Data "forty-four","forty-five","forty-six","forty-seven","forty-eight","forty-nine","fifty","fifty-one","fifty-two","fifty-three"
Data "fifty-four","fifty-five","fifty-six","fifty-seven","fifty-eight","fifty-nine"

Second:
Data "one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen"
Data "fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","twenty-one","twenty-two","twenty-three","twenty-four"
Data "twenty-five","twenty-six","twenty-seven","twenty-eight","twenty-nine","thirty","thirty-one","thirty-two","thirty-three"
Data "thirty-four","thirty-five","thirty-six","thirty-seven","thirty-eight","thirty-nine","forty","forty-one","forty-two","forty-three"
Data "forty-four","forty-five","forty-six","forty-seven","forty-eight","forty-nine","fifty","fifty-one","fifty-two","fifty-three"
Data "fifty-four","fifty-five","fifty-six","fifty-seven","fifty-eight","fifty-nine"

MonthTable:
Data 0,3,3,6,1,4,6,2,5,0,3,5

AllText:
Data "Saturday Sunday Monday Tuesday Wednesday Thursday Friday the first second third fourth fifth sixth seventh eighth ninth tenth eleventh "
Data "twelfth thirteenth fourteenth fifteenth sixteenth seventeenth eighteenth nineteenth twentieth twenty-first twenty-second twenty-third "
Data "twenty-fourth twenty-fifth twenty-sixth twenty-seventh twenty-eighth twenty-ninth thirtieth thirty-first of January February March April "
Data "May June July August September October November December , one two three four five six seven eight nine ten eleven twelve o'clock oh-one "
Data "oh-two oh-three oh-four oh-five oh-six oh-seven oh-eight oh-nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen "
Data "nineteen twenty twenty-one twenty-two twenty-three twenty-four twenty-five twenty-six twenty-seven twenty-eight twenty-nine thirty "
Data "thirty-one thirty-two thirty-three thirty-four thirty-five thirty-six thirty-seven thirty-eight thirty-nine forty forty-one forty-two "
Data "forty-three forty-four forty-five forty-six forty-seven forty-eight forty-nine fifty fifty-one fifty-two fifty-three fifty-four "
Data "fifty-five fifty-six fifty-seven fifty-eight fifty-nine AM PM and precisely one two three four five six seven eight nine ten eleven "
Data "twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty twenty-one twenty-two twenty-three twenty-four twenty-five "
Data "twenty-six twenty-seven twenty-eight twenty-nine thirty thirty-one thirty-two thirty-three thirty-four thirty-five thirty-six "
Data "thirty-seven thirty-eight thirty-nine forty forty-one forty-two forty-three forty-four forty-five forty-six forty-seven forty-eight "
Data "forty-nine fifty fifty-one fifty-two fifty-three fifty-four fifty-five fifty-six fifty-seven fifty-eight fifty-nine second seconds"
Data "EOF"


' End of DATA section


'''''''''''''''''''
' Release History '
'''''''''''''''''''
'
' 1.0.0.5 - May 8, 2022
' First stable build release.
'
' 1.0.0.6 - May 9, 2022
' Added detection for situations where a user might specify a non-existant font in the WordClock.ini file. This can happen if the
' user copies the WordClock.ini file to another system and the font specified does not exist on that system. In that case, the
' program will fall back to the LUCON.TTF font.
'
' In addition, we corrected a situation where the dynamic font sizing for fullscreen mode might use fewer lines of the screen than
' it could. This affected only full screen sizing. This issue has been resolved.
'
' 1.0.0.7 - May 9, 2022
' Because the dynamic font resizing is working so well now, we have removed the commented out sections related to the use of the
' "+" and "-" hotkeys for altering the font size. In addition, all support for specifying the font size in the WordClock.ini
' has been pulled.

