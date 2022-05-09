# Word-Clock
A clock that shows time in written words - My own implementation of  a screensaver by Simon Heys

This project contains two components:

1) A full featured program
2) A limited feature set screensaver

See the appropriate section below (Full Featured Program or Screensaver) for usage instructions. Please also see the sections following both of those. 


 * Full Featured Program

By default, the program will open in full screen mode and will include a digital clock displayed on the last line of the screen. These, as well as other settings, can be altered by hotkeys as well as a file named WordClock.ini.

To use the program, simply run the executable. No installation is required. If you plan to use the WordClock.ini file, place it in the same folder with the program.

In windowed mode, you are free to resize the window. The font size will be dynamically adjusted to make best use of the available space. The program will enforce a minimum screen size of 200 x 200 pixels.

   Hotkeys
   -------

The following hotkeys are available:

NOTE: A hotkey can be either upper case or lower case.

D : Toggle the digital display at the bottom of the screen on and off
F : Toggle in and out of fullscreen mode
H : Display program help
S : Display statistics / current values in use by the program

Any other key will exit the program

   Using the WorkClock.ini File
   ----------------------------

The following are examples of entries that can be placed in the WordClock.ini.
Any settings in this file will override the default settings in the program.

: This is a comment        -  Comments start with a ":" as the first character and are ignored by the program.
: Windowed mode entries
Font:lucon.ttf             -  The font to be used in windowed mode. Font name only, no path.
WindowHorizontal:800       -  Horizontal resolution (width) used in windowed mode.
WindowVertical:600         -  Vertical resolution (height) used in windowed mode.
: Full screen mode entries
FullscreenFont:lucon.ttf   -  The font to be used in fullscreen mode. Font name only, no path.
StartFullscreen:Y          -  Specify Y to start in fullscreen mode, N to start windowed.
: Other entries
ShowDigitalTime:Y          -  Display the digital time on the last line of the screen.
HandleErrors:Y             -  Enables the error handling routies. Disable if you need to see original QB64 error message(s).


* Screensaver

The screensaver operates in fullscreen mode only. It includes a digital clock displayed on the last line of the screen. This can be turned off by using hotkeys as well as a file named WordClockSCR.ini to turn it off by default.

To use the screensaver, follow these steps:

1) Place the WordClock.scr file and the optional WinClockSCR.ini file in its final location. I like to put it in a folder
within My Documents.

2) Right-click the WordClock.scr file and choose "install".

   Hotkeys
   -------

Only one hotkey is available in the screesaver:

NOTE: A hotkey can be either upper case or lower case.

D : Toggle the digital display at the bottom of the screen on and off

Any other key will exit the program


   Using the WorkClockSCR.ini File
   -------------------------------

The following are examples of entries that can be placed in the WordClockSCR.ini. Any settings in this file will override the default settings in the program.

: This is a comment        -  Comments start with a ":" as the first character and are ignored by the program.
: Font selection
FullscreenFont:lucon.ttf   -  The font to be used. Font name only, no path.
:
: Other entries
ShowDigitalTime:Y          -  Display the digital time on the last line of the screen.
HandleErrors:Y             -  Enables the error handling routies. Disable if you need to see original QB64 error message(s).


* Error Handling

If an unhandled proram exception were to ocurr, an error message would typically be displayed and the program would then terminate. At this time, there are no known causes for unhandled exceptions. As a result, the errorhandling code simply restarts the program again from scratch. If you encounter any unexpected restarts and want to see the original error message, set HandleErrors to N to in the .ini file.


* Font Selection

When specifying a font in your WordClock.ini or WordClockSCR.ini file, specify only the name of the font, don't include a path. You can browse available fonts in the %SystemRoot%\fonts folder.

NOTE: %SystemRoot% is typically C:\Windows.

Windows File Explorer uses a special view to show fonts. Note that many fonts have sub-fonts. For example, Ariel is actually a folder within whih you will find multiple fonts within the Ariel family. To get the filename of a font, right-click and choose Properties. The first line under the Genral tab will display the filename that you need.


* Font Sizes on Small Screen

If you drag the window so that it occupies a very small space, you may find that several lines toward the bottom are unoccupied. This is expected behavior. Increasing the font size just one point can increase the number of lines needed to display everything by several lines, overflowing the window.

The larger the window size, the less impact this issue has. For this reason, we limit the window to a minimum size of 200 x 200 pixels.

Bottom line: If you see more than one empty line at the bottom of the screen, rest assured that the dynamic font sizing has chosen the best possible font size.


* Updates
=======

Updates will be available on https://github.com/hsehestedt.


* Future Plans
============

This a "1.0" release of the program. In the future, I hope to make some improvements such as adding te ability to modify colors and maybe allow for other types of information rather than a digital clock to be displayed on the bottom line.
