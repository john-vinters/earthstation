--
--  Copyright (C) 2010 John Vinters
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, see <http://www.gnu.org/licenses/>.
--
--  earthstation-platform.ads	jvinters	8-May-2010
--

pragma License (GPL);

package EarthStation.Platform is

   --  Platform Specific Code

   DIRECTORY_ERROR	: Exception;

   procedure Create_Home_Directory;
   --  creates home directory if it doesn't already exist.
   --  may raise DIRECTORY_ERROR if problems occur.  This should be called
   --  before Create_Keplerian_Elements_Directory and Create_Preferences_Directory.

   procedure Create_Keplerian_Elements_Directory;
   --  creates directory for Keplerian Elements if it doesn't already exist.
   --  may raise DIRECTORY_ERROR if problems occur.

   procedure Create_Preferences_Directory;
   --  creates preferences directory.
   --  may raise DIRECTORY_ERROR if problems occur.

   function Get_Keplerian_Elements_Directory return String;
   --  returns the directory that holds the Keplerian Elements.
   --  this function returns an empty string if the Platform-appropriate
   --  environment variable isn't set.

   function Get_Preferences_Directory return String;
   --  returns the directory that holds the preferences.
   --  this function returns an empty string if the Platform-appropriate
   --  environment variable isn't set.

   function Get_Share_Directory return String;
   --  Returns the directory containing the background image.
   --  On Unix-like systems this will be /usr/local/share/
   --  On Windows this will be %PROGRAMFILES%\earthstation\

   function Home_Directory return String;
   --  returns home directory for application.
   --  On UNIX-Like platforms this is ${HOME}/.earthstation/
   --  On Windows platforms this is %APPDATA%\EarthStation\
   --  If HOME/APPDATA isn't set the an empty string is returned.

end EarthStation.Platform;

