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
--  earthstation-preferences.adb	jvinters	14-May-2010
--

pragma License (GPL);

with Ada.IO_Exceptions;			use Ada.IO_Exceptions;
with Ada.Strings;			use Ada.Strings;
with Ada.Strings.Fixed;			use Ada.Strings.Fixed;
with EarthStation.Platform;		use EarthStation.Platform;

package body EarthStation.Preferences is

   ------------------------------
   -- Get_Groundstation_Height --
   ------------------------------

   function Get_Groundstation_Height (This : in Pref_Data) return Long_Float is
   begin
      return This.Groundstation_Height;
   end Get_Groundstation_Height;

   --------------------------------
   -- Get_Groundstation_Latitude --
   --------------------------------

   function Get_Groundstation_Latitude (This : in Pref_Data) return Long_Float is
   begin
      return This.Groundstation_Latitude;
   end Get_Groundstation_Latitude;

   ---------------------------------
   -- Get_Groundstation_Longitude --
   ---------------------------------

   function Get_Groundstation_Longitude (This : in Pref_Data) return Long_Float is
   begin
      return This.Groundstation_Longitude;
   end Get_Groundstation_Longitude;

   ----------------------------
   -- Get_Groundstation_Name --
   ----------------------------

   function Get_Groundstation_Name (This : in Pref_Data) return String is
   begin
      return To_String (This.Groundstation_Name);
   end Get_Groundstation_Name;

   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float (File : access File_Type) return Long_Float is
      Float_Str			: constant String := Get_String (File, 256);
   begin
      return Long_Float'Value (Float_Str);
   end Get_Long_Float;

   ----------------------------
   -- Get_Selected_Satellite --
   ----------------------------

   function Get_Selected_Satellite (This : in Pref_Data) return String is
   begin
      return To_String (This.Selected_Satellite);
   end Get_Selected_Satellite;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (File			: access File_Type;
      Max_Length		: in Positive := 256) return String
   is
      Line_Length		: Natural;
      Temp_String		: String (1 .. Max_Length);
   begin
      Get_Line (File.all, Temp_String, Line_Length);
      return Trim (Temp_String (1 .. Line_Length), Both);
   end Get_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Pref_Data) is
   begin
      Set_Groundstation_Height (This, 500.0);
      Set_Groundstation_Latitude (This, 53.73);
      Set_Groundstation_Longitude (This, -1.86);
      Set_Groundstation_Name (This, "Halifax");
      Set_Selected_Satellite (This, "");
      Create_Home_Directory;
      Create_Preferences_Directory;
      Load_Preferences (This);
   exception
      when Ada.IO_Exceptions.NAME_ERROR =>
         Save_Preferences (This);
      when EarthStation.Platform.DIRECTORY_ERROR =>
         raise PREF_EXCEPTION;
   end Initialize;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (This : in out Pref_Data) is
      Closed		: Boolean := True;
      File		: aliased File_Type;
      Prefs_Name	: constant String :=
        EarthStation.Platform.Get_Preferences_Directory & "prefs";
   begin
      Open (File, In_File, Prefs_Name);
      Closed := False;

      Set_Groundstation_Name (This, Get_String (File'Access, NAME_LEN_MAX));
      Set_Groundstation_Latitude (This, Get_Long_Float (File'Access));
      Set_Groundstation_Longitude (This, Get_Long_Float (File'Access));
      Set_Groundstation_Height (This, Get_Long_Float (File'Access));
      Set_Selected_Satellite (This, Get_String (File'Access, 256));

      Close (File);
   exception
      when others =>
         if not Closed then 
            Close (File);
         end if;
         raise;
   end Load_Preferences;

   --------------------
   -- Put_Long_Float --
   --------------------

   procedure Put_Long_Float
     (This		: in out File_Type;
      LF		: in     Long_Float)
   is
      type LR is delta 0.001 digits 8;
      LFV		: constant LR := LR (LF);
      LFS		: constant String := LR'Image (LFV);
   begin
      Put_Line (This, LFS);
   end Put_Long_Float;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String
     (This		: in out File_Type;
      Str		: in     String)
   is
   begin
      Put_Line (This, Str);
   end Put_String;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences (This : in     Pref_Data) is
      Closed		: Boolean := True;
      File		: aliased File_Type;
      Prefs_Name	: constant String :=
        EarthStation.Platform.Get_Preferences_Directory & "prefs";
   begin
      Create (File, Out_File, Prefs_Name);
      Closed := False;

      Put_String (File, Get_Groundstation_Name (This));
      Put_Long_Float (File, This.Groundstation_Latitude);
      Put_Long_Float (File, This.Groundstation_Longitude);
      Put_Long_Float (File, This.Groundstation_Height);
      Put_String (File, Get_Selected_Satellite (This));

      Close (File);
   exception
      when others =>
         if not Closed then
            Close (File);
         end if;
         raise;
   end Save_Preferences;

   ------------------------------
   -- Set_Groundstation_Height --
   ------------------------------

   procedure Set_Groundstation_Height
     (This			: in out Pref_Data;
      Groundstation_Height	: in      Long_Float)
   is
   begin
      This.Groundstation_Height := Groundstation_Height;
   end Set_Groundstation_Height;

   --------------------------------
   -- Set_Groundstation_Latitude --
   --------------------------------

   procedure Set_Groundstation_Latitude
     (This			: in out Pref_Data;
      Groundstation_Latitude	: in      Long_Float)
   is
   begin
      This.Groundstation_Latitude := Groundstation_Latitude;
   end Set_Groundstation_Latitude;

   ---------------------------------
   -- Set_Groundstation_Longitude --
   ---------------------------------

   procedure Set_Groundstation_Longitude
     (This			: in out Pref_Data;
      Groundstation_Longitude	: in     Long_Float)
   is
   begin
      This.Groundstation_Longitude := Groundstation_Longitude;
   end Set_Groundstation_Longitude;

   ----------------------------
   -- Set_Groundstation_Name --
   ----------------------------

   procedure Set_Groundstation_Name
     (This			: in out Pref_Data;
      Groundstation_Name	: in     String)
   is
   begin
      if Groundstation_Name'Length > NAME_LEN_MAX then
         raise PREF_EXCEPTION;
      else
         Set_Unbounded_String (This.Groundstation_Name, Groundstation_Name);
      end if;
   end Set_Groundstation_Name;

   ----------------------------
   -- Set_Selected_Satellite --
   ----------------------------

   procedure Set_Selected_Satellite
     (This			: in out Pref_Data;
      Satellite_Name		: in     String)
   is
   begin
      Set_Unbounded_String (This.Selected_Satellite, Satellite_Name);
   end Set_Selected_Satellite;

end EarthStation.Preferences;

