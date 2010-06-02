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
--  earthstation-platform.adb	jvinters	8-May-2010
--

pragma License (GPL);

with GNAT.Directory_Operations;		use GNAT.Directory_Operations;
with GNAT.OS_Lib;			use GNAT.OS_Lib;

package body EarthStation.Platform is

   On_Windows	: constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

   ---------------------------
   -- Create_Home_Directory --
   ---------------------------

   procedure Create_Home_Directory is
      Home_Dir	: constant String := Home_Directory;
   begin
      if Home_Dir /= "" then
         begin
            if not Is_Directory (Home_Dir) then
               Make_Dir (Home_Dir);
            end if;
         exception
            when others =>
               raise DIRECTORY_ERROR;
         end;
      else
         raise DIRECTORY_ERROR;
      end if;
   end Create_Home_Directory;

   -----------------------------------------
   -- Create_Keplerian_Elements_Directory --
   -----------------------------------------

   procedure Create_Keplerian_Elements_Directory is
      Keps_Dir	: constant String := Get_Keplerian_Elements_Directory;
   begin
      if Keps_Dir /= "" then
         begin
            if not Is_Directory (Keps_Dir) then
               Make_Dir (Keps_Dir);
            end if;
         exception
            when others =>
               raise DIRECTORY_ERROR;
         end;
      else
         raise DIRECTORY_ERROR;
      end if;
   end Create_Keplerian_Elements_Directory;

   ----------------------------------
   -- Create_Preferences_Directory --
   ----------------------------------

   procedure Create_Preferences_Directory is
      Prefs_Dir	: constant String := Get_Preferences_Directory;
   begin
      if Prefs_Dir /= "" then
         begin
            if not Is_Directory (Prefs_Dir) then
               Make_Dir (Prefs_Dir);
            end if;
         exception
            when others =>
               raise DIRECTORY_ERROR;
         end;
      else
         raise DIRECTORY_ERROR;
      end if;
   end Create_Preferences_Directory;

   --------------------------------------
   -- Get_Keplerian_Elements_Directory --
   --------------------------------------

   function Get_Keplerian_Elements_Directory return String is
      Home	: constant String := Home_Directory;
   begin
      if Home /= "" then
         if On_Windows then
            return Home & "Keps\";
         else
            return Home & "keps/";
         end if;
      else
         return "";
      end if;
   end Get_Keplerian_Elements_Directory;

   -------------------------------
   -- Get_Preferences_Directory --
   -------------------------------

   function Get_Preferences_Directory return String is
      Home	: constant String := Home_Directory;
   begin
      if Home /= "" then
         if On_Windows then
            return Home & "Preferences\";
         else
            return Home & "preferences/";
         end if;
      else
         return "";
      end if;
   end Get_Preferences_Directory;

   -------------------------
   -- Get_Share_Directory --
   -------------------------

   function Get_Share_Directory return String is
   begin
      if On_Windows then
         declare
            Program_Files	: String_Access := Getenv ("PROGRAMFILES");
         begin
            if Program_Files /= null then
               declare
                  Result	: constant String := Program_Files.all &
                                    "\EarthStation\";
               begin
                  Free (Program_Files);
                  return Result;
               end;
            else
               return "";
            end if;
         end;
      else
         return "/usr/local/share/earthstation/";
      end if;
   end Get_Share_Directory;

   --------------------
   -- Home_Directory --
   --------------------

   function Home_Directory return String is
   begin
      if On_Windows then
         declare
            Profile_Directory	: String_Access := Getenv ("APPDATA");
         begin
            if Profile_Directory /= null then
               declare
                  Result	: constant String := Profile_Directory.all
                    & "\EarthStation\";
               begin
                  Free (Profile_Directory);
                  return Result;
               end;
            else
               return "";
            end if;
         end;
      else
         declare
            Home_Directory	: String_Access := Getenv ("HOME");
         begin
            if Home_Directory /= null then
               declare
                  Result	: constant String := Home_Directory.all
                    & "/.earthstation/";
               begin
                  Free (Home_Directory);
                  return Result;
               end;
            else
               return "";
            end if;
         end;
      end if;
   end Home_Directory;

end EarthStation.Platform;

