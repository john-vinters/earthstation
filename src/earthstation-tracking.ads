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
--  earthstation-tracking.ads	jvinters	7-May-2010
--

pragma License (GPL);

with Ada.Calendar;			use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with EarthStation.Data_Table;		use EarthStation.Data_Table;
with EarthStation.Map_Display;		use EarthStation.Map_Display;
with EarthStation.Predict;		use EarthStation.Predict;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Menu;				use Gtk.Menu;
with Gtk.Menu_Item;			use Gtk.Menu_Item;

package EarthStation.Tracking is

   type Data is private;
   type Data_Access is access all Data;

   package Menu_Item_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, Data_Access);

   procedure Add_Satellite
     (This			: in out Data;
      Id			: in     String;
      Elements			: in     EarthStation.Predict.Keplerian_Elements);
   --  Adds a Satellite to the tracking list

   function Allocate_Track_Menu
     (This			: in Data_Access;
      Handler			: in Menu_Item_Callback.Marshallers.Void_Marshaller.Handler)
     return Gtk_Menu;
   --  Allocates a new Track Menu; should be called after calls to Add_Satellite
   --  or Clear.

   procedure Clear (This : in out Data);
   --  Clears list of tracked Satellites

   function Get_Selected_Satellite (This : in Data) return String;
   --  Returns Id of currently selected Satellite (or empty string if none)

   procedure Initialize
     (This			:    out Data;
      Groundstation_Name	: in     String;
      Latitude			: in     Long_Float;
      Longitude			: in     Long_Float;
      Height			: in     Long_Float);
   --  Initializes tracking - sets up groundstation details

   procedure Select_Satellite
     (This			: in out Data;
      Menu_Item			: access Gtk_Menu_Item_Record'Class);
   --  Selects a Satellite given a Menu Item.  If Satellite can't be found,
   --  then the current selection is left as-is.     

   procedure Select_Satellite
     (This			: in out Data;
      Satellite_Id		: in     String);
   --  Selects a Satellite given it's Id; if Satellite can't be found, then
   --  the current selection is left as-is.

   procedure Update_Display
     (This			: in out Data;
      Map_Display		: access Map_Display_Record'Class;
      Table_Display		: access Data_Table_Record'Class);
   --  Updates display - draws Satellite footprints and updates Data Table
   --  with currently selected Satellite data

private

   type Orbit_State is (FIND_AOS, FIND_LOS);

   type Satellite_Data is record
      Max_Elevation		: Long_Float := 0.0;
      Menu_Item			: Gtk_Menu_Item;
      Next_AOS			: Ada.Calendar.Time;
      Next_Check		: Ada.Calendar.Time;
      Next_LOS			: Ada.Calendar.Time;
      Rng_Vectors		: EarthStation.Predict.Range_Vectors;
      Sat_Vectors		: EarthStation.Predict.Satellite_Vectors;
      Satellite			: EarthStation.Predict.Satellite;
      Satellite_Id		: Unbounded_String;
   end record;

   procedure Calculate_Next_AOS_LOS
     (This			: in     Data;
      Info			: in out Satellite_Data);
   --  Calculates next Satellite AOS and LOS

   function Compare_Elements
     (Left			: in Satellite_Data;
      Right			: in Satellite_Data) return Boolean;

   package Satellite_Vector is new Ada.Containers.Vectors
     (Index_Type	=> Positive,
      Element_Type	=> Satellite_Data,
      "="		=> Compare_Elements);

   type Data is record
      Groundstation		: Earthstation.Predict.Observer;
      Groundstation_Name	: Unbounded_String;
      Last_Update		: Ada.Calendar.Time;
      Latitude			: Long_Float;
      Longitude			: Long_Float;
      Satellites		: Satellite_Vector.Vector;
      Selected_Satellite	: Natural := 0;
   end record;

end EarthStation.Tracking;

