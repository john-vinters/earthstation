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
--  earthstation-data_table.ads	jvinters	3-May-2010
--

pragma License (GPL);

with Ada.Calendar;			use Ada.Calendar;
with Glib;				use Glib;
with Gtk.Label;				use Gtk.Label;
with Gtk.Table;				use Gtk.Table;

package EarthStation.Data_Table is

   type Data_Table_Record is new Gtk.Table.Gtk_Table_Record with private;
   type Data_Table is access all Data_Table_Record'Class;

   procedure Gtk_New (This : out Data_Table);

   procedure Initialize (This : access Data_Table_Record'Class);

   procedure Set_Altitude
     (This		: access Data_Table_Record'Class;
      Altitude		: in     Long_Float);

   procedure Set_Azimuth
     (This		: access Data_Table_Record'Class;
      Azimuth           : in     Long_Float);

   procedure Set_Elevation
     (This		: access Data_Table_Record'Class;
      Elevation		: in     Long_Float);

   procedure Set_Latitude
     (This		: access Data_Table_Record'Class;
      Latitude		: in     Long_Float);

   procedure Set_Longitude
     (This		: access Data_Table_Record'Class;
      Longitude		: in     Long_Float);

   procedure Set_Max_Elevation
     (This		: access Data_Table_Record'Class;
      Elevation		: in     Long_Float);

   procedure Set_Next_AOS
     (This		: access Data_Table_Record'Class;
      Next_AOS		: in     Ada.Calendar.Time);

   procedure Set_Next_LOS
     (This		: access Data_Table_Record'Class;
      Next_LOS		: in     Ada.Calendar.Time);

   procedure Set_Orbit
     (This		: access Data_Table_Record'Class;
      Orbit		: in     Long_Integer);

   procedure Set_Range
     (This		: access Data_Table_Record'Class;
      Satellite_Range	: in     Long_Float);

   procedure Set_Range_Rate
     (This		: access Data_Table_Record'Class;
      Range_Rate	: in     Long_Float);

   procedure Set_Satellite_Id
     (This		: access Data_Table_Record'Class;
      Id		: in     String);

   procedure Set_Visibility
     (This		: access Data_Table_Record'Class;
      Visibility	: in     String);

private

   type Data_Table_Record is new Gtk.Table.Gtk_Table_Record with record
      Altitude		: Gtk_Label;
      Azimuth		: Gtk_Label;
      Elevation		: Gtk_Label;
      Max_Elevation	: Gtk_Label;
      Next_AOS		: Gtk_Label;
      Next_LOS		: Gtk_Label;
      Orbit		: Gtk_Label;
      Range_Rate	: Gtk_Label;
      Satellite_Id	: Gtk_Label;
      Satellite_Range	: Gtk_Label;
      Satellite_Table	: Gtk_Table;
      Subsat_Latitude	: Gtk_Label;
      Subsat_Longitude	: Gtk_Label;
      Visibility	: Gtk_Label;
   end record;

   procedure Attach_Label
     (This		: access Data_Table_Record'Class;
      Label		: in    String;
      Left		: in    Guint;
      Right		: in    Guint;
      Top		: in    Guint;
      Bottom		: in    Guint);
   --  Sets up a new label

   procedure Setup_Label
     (This		: access Data_Table_Record'Class;
      Label		: in     String;
      Display_Element	:    out Gtk_Label;
      Left		: in     Guint;
      Top               : in     Guint);
   --  Sets up Label and Display Element

end EarthStation.Data_Table;

