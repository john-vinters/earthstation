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
--  earthstation-preferences.ads	jvinters	14-May-2010
--

pragma License (GPL);

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;

package EarthStation.Preferences is

   PREF_EXCEPTION		: Exception;

   NAME_LEN_MAX			: constant := 256;

   type Pref_Data is private;

   function Get_Groundstation_Height (This : in Pref_Data) return Long_Float;
   --  Returns the currently set Groundstation Height.

   function Get_Groundstation_Latitude (This : in Pref_Data) return Long_Float;
   --  Returns the currently set Groundstation Latitude.

   function Get_Groundstation_Longitude (This : in Pref_Data) return Long_Float;
   --  Returns the currently set Groundstation Longitude.

   function Get_Groundstation_Name (This : in Pref_Data) return String;
   --  Returns the currently set Groundstation Name.

   function Get_Selected_Satellite (This : in Pref_Data) return String;
   --  Returns the name of the currently selected Satellite.

   procedure Initialize (This : in out Pref_Data);
   --  Tries to load preferences from file.  If the file doesn't exist, then
   --  we set some default values.

   procedure Load_Preferences (This : in out Pref_Data);
   --  Loads preference data.  Raises PREF_EXCEPTION if there was an error
   --  reading the preferences file (or if it doesn't exist)

   procedure Save_Preferences (This : in out Pref_Data);
   --  Saves preference data.  Raises PREF_EXCEPTION if we couldn't successfully
   --  save the preferences file.

   procedure Set_Groundstation_Height
     (This			: in out Pref_Data;
      Groundstation_Height	: in     Long_Float);
   --  Sets the Groundstation Height.

   procedure Set_Groundstation_Latitude
     (This			: in out Pref_Data;
      Groundstation_Latitude	: in     Long_Float);
   --  Sets the Groundstation Latitude.

   procedure Set_Groundstation_Longitude
     (This			: in out Pref_Data;
      Groundstation_Longitude	: in     Long_Float);
   --  Sets the Groundstation Longitude.

   procedure Set_Groundstation_Name
     (This			: in out Pref_Data;
      Groundstation_Name	: in     String);
   --  Sets the Groundstation Name.  PREF_EXCEPTION is raised if the name
   --  is longer than NAME_LEN_MAX characters.

   procedure Set_Selected_Satellite
     (This			: in out Pref_Data;
      Satellite_Name		: in     String);
   --  Sets the currently selected satellite name.

private

   type Pref_Data is record
      Groundstation_Height	: Long_Float;
      Groundstation_Latitude	: Long_Float;
      Groundstation_Longitude	: Long_Float;
      Groundstation_Name	: Unbounded_String;
      Selected_Satellite	: Unbounded_String;
   end record;

   function Get_Long_Float (File : access File_Type) return Long_Float;
   --  Reads a Long_Float from input.

   function Get_String
     (File			: access File_Type;
      Max_Length		: in Positive := 256) return String;
   --  Reads a String from input.

   procedure Put_Long_Float
     (This		: in out File_Type;
      LF		: in     Long_Float);
   --  Writes a Long_Float.

   procedure Put_String
     (This		: in out File_Type;
      Str		: in     String);
   --  Writes a String.

end EarthStation.Preferences;

