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
--  earthstation-data_table.adb	jvinters	3-May-2010
--

pragma License (GPL);

with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Gtk.Enums;				use Gtk.Enums;

package body EarthStation.Data_Table is

   ------------------
   -- Attach_Label --
   ------------------

   procedure Attach_Label
     (This		: access Data_Table_Record'Class;
      Label		: in    String;
      Left		: in    Guint;
      Right		: in    Guint;
      Top		: in    Guint;
      Bottom		: in    Guint)
   is
      New_Label		: Gtk.Label.Gtk_Label;
      Options		: constant Gtk_Attach_Options := Expand or Shrink or Fill;
   begin
      Gtk_New (New_Label, "<span weight=""bold"">" & Label & "</span>");
      Set_Use_Markup (New_Label, True);
      Set_Alignment (New_Label, 1.0, 0.5);
      Attach (This, New_Label, Left, Right, Top, Bottom, Options);
   end Attach_Label;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (This : out Data_Table) is
   begin
      This := new Data_Table_Record;
      Initialize (This);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Data_Table_Record'Class) is
      Options	: constant Gtk_Attach_Options := Expand or Shrink or Fill;
   begin
      Gtk.Table.Initialize (This, 3, 10, Homogeneous => False);
      Set_Col_Spacings (This, 4);
      Set_Row_Spacings (This, 5);

      --  First Row
      Setup_Label (This, "Satellite:", This.Satellite_Id, 0, 0);
      Setup_Label (This, "Azimuth:", This.Azimuth, 2, 0);
      Setup_Label (This, "Elevation:", This.Elevation, 4, 0);
      Setup_Label (This, "Range:", This.Satellite_Range, 6, 0);
      Setup_Label (This, "Altitude:", This.Altitude, 8, 0);

      --  Second Row
      Setup_Label (This, "Orbit:", This.Orbit, 0, 1);
      Setup_Label (This, "Latitude:", This.Subsat_Latitude, 2, 1);
      Setup_Label (This, "Longitude:", This.Subsat_Longitude, 4, 1);
      Setup_Label (This, "Range Rate:", This.Range_Rate, 6, 1);
      Setup_Label (This, "Visibility:", This.Visibility, 8, 1);

      --  Third Row
      Attach_Label (This, "Next AOS:", 0, 1, 2, 3);
      Gtk_New (This.Next_AOS, " -");
      Set_Alignment (This.Next_AOS, 0.0, 0.5);
      Attach (This, This.Next_AOS, 1, 4, 2, 3, Options);
      Attach_Label (This, "Next LOS:", 4, 5, 2, 3);
      Gtk_New (This.Next_LOS, " -");
      Set_Alignment (This.Next_LOS, 0.0, 0.5);
      Attach (This, This.Next_LOS, 5, 8, 2, 3, Options);
      Setup_Label (This, "Max Elevation:", This.Max_Elevation, 8, 2);
   end Initialize;

   ------------------
   -- Set_Altitude --
   ------------------

   procedure Set_Altitude
     (This		: access Data_Table_Record'Class;
      Altitude		: in     Long_Float)
   is
      Altitude_Integer	: constant Long_Integer := Long_Integer (Altitude);
      Altitude_String	: constant String := Long_Integer'Image (Altitude_Integer)
                            & "km";
   begin
      Set_Label (This.Altitude, Altitude_String);
   end Set_Altitude;

   -----------------
   -- Set_Azimuth --
   -----------------

   procedure Set_Azimuth
     (This		: access Data_Table_Record'Class;
      Azimuth           : in     Long_Float)
   is
      type AzR is delta 0.1 digits 8;
      Azimuth_Real	: constant AzR := AzR (Azimuth);
      Azimuth_String	: constant String := AzR'Image (Azimuth_Real)
                            & "<span size=""xx-small"" rise=""5000"">o</span>";
   begin
      Set_Label (This.Azimuth, Azimuth_String);
   end Set_Azimuth;

   -------------------
   -- Set_Elevation --
   -------------------

   procedure Set_Elevation
     (This		: access Data_Table_Record'Class;
      Elevation		: in     Long_Float)
   is
      type ElR is delta 0.1 digits 8;
      Elevation_Real	: constant ElR := ElR (Elevation);
      Elevation_String	: constant String := ElR'Image (Elevation_Real)
                            & "<span size=""xx-small"" rise=""5000"">o</span>";
   begin
      Set_Label (This.Elevation, Elevation_String);
   end Set_Elevation;

   ------------------
   -- Set_Latitude --
   ------------------

   procedure Set_Latitude
     (This		: access Data_Table_Record'Class;
      Latitude		: in     Long_Float)
   is
      type LatR is delta 0.1 digits 8;
      Latitude_Real	: constant LatR := LatR (Latitude);
      Latitude_String	: constant String := LatR'Image (Latitude_Real)
                            & "<span size=""xx-small"" rise=""5000"">o</span>";
   begin
      Set_Label (This.Subsat_Latitude, Latitude_String);
   end Set_Latitude;

   -------------------
   -- Set_Longitude --
   -------------------

   procedure Set_Longitude
     (This		: access Data_Table_Record'Class;
      Longitude		: in     Long_Float)
   is
      type LonR is delta 0.1 digits 8;
      Longitude_Real	: constant LonR := LonR (Longitude);
      Longitude_String	: constant String := LonR'Image (Longitude_Real)
                            & "<span size=""xx-small"" rise=""5000"">o</span>";
   begin
      Set_Label (This.Subsat_Longitude, Longitude_String);
   end Set_Longitude;

   -----------------------
   -- Set_Max_Elevation --
   -----------------------

   procedure Set_Max_Elevation
     (This		: access Data_Table_Record'Class;
      Elevation		: in     Long_Float)
   is
      type ElR is delta 0.1 digits 8;
      Elevation_Real	: constant ElR := ElR (Elevation);
      Elevation_String	: constant String := ElR'Image (Elevation_Real)
                            & "<span size=""xx-small"" rise=""5000"">o</span>";
   begin
      Set_Label (This.Max_Elevation, Elevation_String);
   end Set_Max_Elevation;

   ------------------
   -- Set_Next_AOS --
   ------------------

   procedure Set_Next_AOS
     (This		: access Data_Table_Record'Class;
      Next_AOS		: in     Ada.Calendar.Time)
   is
      Now		: constant Time := Clock;
   begin
      if Now < Next_AOS then
         Set_Label (This.Next_AOS, " " & Image (Next_AOS));
      else
         Set_Label (This.Next_AOS, " -");
      end if;
   end Set_Next_AOS;

   ------------------
   -- Set_Next_LOS --
   ------------------

   procedure Set_Next_LOS
     (This		: access Data_Table_Record'Class;
      Next_LOS		: in     Ada.Calendar.Time)
   is
      Now		: constant Time := Clock;
   begin
      if Now < Next_LOS then
         Set_Label (This.Next_LOS, " " & Image (Next_LOS));
      else
         Set_Label (This.Next_LOS, " -");
      end if;
   end Set_Next_LOS;

   ---------------
   -- Set_Orbit --
   ---------------

   procedure Set_Orbit
     (This		: access Data_Table_Record'Class;
      Orbit		: in     Long_Integer)
   is
      Orbit_String	: constant String := Long_Integer'Image (Orbit);
   begin
      Set_Label (This.Orbit, Orbit_String);
   end Set_Orbit;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (This		: access Data_Table_Record'Class;
      Satellite_Range	: in     Long_Float)
   is
      Range_Integer	: constant Long_Integer := Long_Integer (Satellite_Range);
      Range_String	: constant String := Long_Integer'Image (Range_Integer)
                            & "km";
   begin
      Set_Label (This.Satellite_Range, Range_String);
   end Set_Range;

   --------------------
   -- Set_Range_Rate --
   --------------------

   procedure Set_Range_Rate
     (This		: access Data_Table_Record'Class;
      Range_Rate	: in     Long_Float)
   is
      type RaR is delta 0.1 digits 8;
      Range_Rate_Real	: constant RaR := RaR (Range_Rate);
      Range_Rate_String	: constant String := RaR'Image (Range_Rate_Real) & "km/s";
   begin
      Set_Label (This.Range_Rate, Range_Rate_String);
   end Set_Range_Rate;

   ----------------------
   -- Set_Satellite_Id --
   ----------------------

   procedure Set_Satellite_Id
     (This		: access Data_Table_Record'Class;
      Id		: in     String)
   is
   begin
      Set_Label (This.Satellite_Id, ' ' & Id);
   end Set_Satellite_Id;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (This		: access Data_Table_Record'Class;
      Visibility	: in     String)
   is
   begin
      Set_Label (This.Visibility, ' ' & Visibility);
   end Set_Visibility;

   -----------------
   -- Setup_Label --
   -----------------

   procedure Setup_Label
     (This		: access Data_Table_Record'Class;
      Label		: in     String;
      Display_Element	:    out Gtk_Label;
      Left		: in     Guint;
      Top               : in     Guint)
   is
      Options		: constant Gtk_Attach_Options := Expand or Shrink or Fill;
   begin
      Attach_Label (This, Label, Left, Left + 1, Top, Top + 1);
      Gtk_New (Display_Element, " -");
      Set_Use_Markup (Display_Element, True);
      Set_Alignment (Display_Element, 0.0, 0.5);
      Attach (This, Display_Element, Left + 1, Left + 2, Top, Top + 1, Options);
   end Setup_Label;

end EarthStation.Data_Table;

