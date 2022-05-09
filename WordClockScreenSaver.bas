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

Dim AllText As String ' Contains a copy of all text that will be displayed on screen. Used to test font sizes.
Dim AM_PM As String ' This flag will be set to either "AM" or "PM"
Dim CharPerLine As Integer ' Number of characters that can fit on a line at a given screen and font size
Dim CharPosition As Integer ' Used to keep track of character positioning while manipulating strings
Dim CurrentDate As String ' Hold the entire date (Month / Day / Year) as a string
Dim CurrentTime As String ' Hold the entire time (Hours / Minutes / Seconds) as a string
Dim Day As Integer ' Day of the month (1-31) as a number
Dim DayOfWeek As Integer ' Day of the week (1-7) as a number
Dim DayOfWeekString(7) As String ' An array holding each day of the week as an English word
Dim DayString(31) As String ' An array holding each day of the month (1-31) as a string
Dim Decade As Integer ' The numerical value of the last 2 digits of the year
Dim DigitalHour As String ' Hours converted to a 2 digital string
Dim ff As Integer ' Used to get a freefile number when opening the .ini file
Dim FileLine As String ' Holds one line of text at a time read from the .ini file
Dim Font As Long ' Handle to a font
Dim FontSizeToTest As Integer ' In the routine that tests fit of a font on the screen, this holds the current font size to be tested
Dim FontTooLarge As String ' Flag to indicate when a font is too large to properly display all text
Dim FullscreenFontHeight As Integer
Dim FullscreenFontPath As String ' The name of the font, with path, used in fullscreen mode
Dim FullscreenFontSize As Integer ' The fontsize used in fullscreen mode
Dim FullscreenFontWidth As Integer ' Width of a font in fullscreen mode
Dim handle As Long ' Stores a handle to the screen
Dim HandleErrors As String ' If set to "Y" then error handling is enabled, otherwise it is disabled
Dim High As Integer ' The height of a font undergoing testing to see if it allows text to properly fit on screen
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
Dim MouseCount ' Counts the number of mouse events
Dim OldSecond As Integer ' A variable that is used to determine if the seconds have changed from the last time we checked
Dim Result1 As Integer ' A temporary variable
Dim Result2 As Integer ' A temporary variable
Dim Result3 As Integer ' A temporary variable
Dim Second As Integer ' The current seconds as a number (0-59)
Dim SecondString(59) As String ' The current seconds as an English word from one through fifty-nine
Dim ShowDigitalTime As String ' Set to "Y" to show digital time on the last line of the screen
Dim Temp As Integer ' A temporary variable
Dim Temp2 As Integer ' A temporary variable
Dim TempString As String ' A temporary string of characters, used mainly in string manipulation routines
Dim Wide As Integer ' The width of a font undergoing testing to see if it allows text to properly fit on screen
Dim x As Integer ' General purpose counter used in FOR...NEXT loops
Dim Year As Integer ' Stores the current year

' Default values used for entries not available from a .ini file and initialization of other variables
' NOTE: The system adjusts the font size autmatically. We set an initial size for FullscreenFontSize just
' to serve as a starting point for the dynamic font sizing routine.

FullscreenFontPath$ = Environ$("SYSTEMROOT") + "\fonts\lucon.ttf"
FullscreenFontSize = 35
ShowDigitalTime$ = "Y"
AllText$ = "" ' Initialize the string to an empty state
HandleErrors$ = "Y"

' If a .ini file exists, open it and parse it. Values found in the .ini will override the defaults
' defined above.

If _FileExists("WordClockSCR.ini") Then

    ff = FreeFile
    Open "WordClockSCR.ini" For Input As #ff

    Do Until EOF(ff)
        Line Input #ff, FileLine$

        ' If line starts with a colon (:), it is a comment. Ignore it.

        If Left$(FileLine$, 1) = ":" Then _Continue

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

        ' If line starts with "FONT:", We will use the font specified. Enter the name of the path (ex. Lucon.TTF), no path.

        If UCase$(Left$(FileLine$, 15)) = "FULLSCREENFONT:" Then
            TempString$ = Environ$("SYSTEMROOT") + "\fonts\" + Right$(FileLine$, (Len(FileLine$) - 15))
            If _FileExists(TempString$) Then FullscreenFontPath$ = TempString$
        End If

    Loop

    Close #ff

    ' If HandleErrors$ is set to "Y" then enable error handling

    If HandleErrors$ = "Y" Then
        On Error GoTo HandleErrors
    End If

End If

' Setup screen for fullscreen

_ScreenHide
handle& = _NewImage(_DesktopWidth, _DesktopHeight, 256)
Screen handle&
_Delay .5
_FullScreen
GoSub FindLargestFontSize
Font& = _LoadFont(FullscreenFontPath$, FullscreenFontSize, "MONOSPACE")
_Font Font&
High = _FontHeight
Wide = _FontWidth
MaxLines = Int(_DesktopHeight / High)
FullscreenFontWidth = _FontWidth
FullscreenFontHeight = _FontHeight

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

_ScreenShow

' Clear the keyboard buffer before we enter the main program loop.

Do While InKey$ <> ""
Loop

Do
Loop While _MouseInput

_MouseHide

' This is the main loop that retrieves the date and time, breaks it down into individual components, and then
' displays the time and date in words.

Do
    _Limit 60 ' Limit the number of times that we perform this loop to a maximum of 60 iterations per second

    ' The lines below check for any keypresses. If a hotkey is pressed, then we take the appropriate action.
    ' Pressing any other key will exit the program. This is most useful when the program is being used as a
    ' screensaver.

    ' Check mouse events. If mouse events occur, exit from the screensaver. Note that we get a couple spurios event at the outset we
    ' keep track of mouse events and won't exit until we get a few.

    Do While _MouseInput
        MouseCount = MouseCount + 1
        If MouseCount = 50 Then System
    Loop

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
        Case Else
            _MouseShow
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

    CharPerLine = Int(_DesktopWidth / Wide)

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


FindLargestFontSize:


' This subroutine will determine what the largest font size is that all allow all text to be displayed on the screen.
' This subroutine will in turn call the subroutine "FontSizeTest".

' We will begin our testing with the currently set font size

FontSizeToTest = FullscreenFontSize

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

FullscreenFontSize = FontSizeToTest

Return


FontSizeTest:


Font& = _LoadFont(FullscreenFontPath$, FontSizeToTest, "MONOSPACE")
_Font Font&
High = _FontHeight
Wide = _FontWidth
CharPerLine = Int(_DesktopWidth / Wide)
MaxLines = Int(_DesktopHeight / High)

' We are reserving the last line for use by the Digital Clock option, so we are subtracting 1 line from MaxLines

MaxLines = MaxLines - 1

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
' Add the ability to specify a different font via the .ini file
'
' 1.0.0.7 - May 9, 2022
' No change in functionality, just some update to comments to make some items easier to understand.

