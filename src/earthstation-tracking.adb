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
--  earthstation-tracking.adb	jvinters	7-May-2010
--

pragma License (GPL);

with Gdk.Color;				use Gdk.Color;
with Gtk.Widget;			use Gtk.Widget;

package body EarthStation.Tracking is

   use Satellite_Vector;

   Colour_Configured		: Boolean := False;
   Groundstation_Colour		: Gdk.Color.Gdk_Color;
   Inrange_Colour		: Gdk.Color.Gdk_Color;
   Max_Tries			: constant Natural := 8640;
   Minimum_Elevation		: constant Long_Float := 3.0;
   Null_Time			: constant Time := Clock;

   -------------------
   -- Add_Satellite --
   -------------------

   procedure Add_Satellite
     (This			: in out Data;
      Id			: in     String;
      Elements			: in     EarthStation.Predict.Keplerian_Elements)
   is
      Item	: Satellite_Data;
   begin
      EarthStation.Predict.Initialize_Satellite
        (This		=> Item.Satellite,
         Elements	=> Elements);
      Set_Unbounded_String (Item.Satellite_Id, Id);

      Calculate_Next_AOS_LOS (This, Item);
      Append (This.Satellites, Item);
      This.Selected_Satellite := 1;
   end Add_Satellite;

   ----------------------------
   -- Calculate_Next_AOS_LOS --
   ----------------------------

   procedure Calculate_Next_AOS_LOS
     (This			: in     Data;
      Info			: in out Satellite_Data)
   is
      Dn	: Long_Integer;
      Df	: Long_Float;
      Ele	: Long_Float;
      State	: Orbit_State := FIND_AOS;
      T         : Ada.Calendar.Time := Clock;
      Tries	: Natural := 0;
   begin
      Info.Next_AOS := Null_Time;
      Info.Next_LOS := Null_Time;

      loop
         EarthStation.Predict.Clock_To_Day (T, Dn, Df);
         Info.Sat_Vectors := EarthStation.Predict.Calculate_Satellite_Vectors
           (Info.Satellite, Dn, Df);
         Info.Rng_Vectors := EarthStation.Predict.Calculate_Range_Vectors
           (This.Groundstation, Info.Sat_Vectors);

         Ele := EarthStation.Predict.Get_Elevation (Info.Rng_Vectors);
         if State = FIND_AOS and then Ele > Minimum_Elevation then
            Info.Max_Elevation := 0.0;
            Info.Next_AOS := T;
            State := FIND_LOS;
         elsif State = FIND_LOS and then Ele < Minimum_Elevation then
            Info.Next_LOS := T;
            exit;
         end if;

         if State = FIND_LOS and then Ele > Info.Max_Elevation then
            Info.Max_Elevation := Ele;
         end if;

         Tries := Tries + 1;
         T := T + Duration (10.0);
         exit when Tries > Max_Tries;
      end loop;

      Info.Next_Check := T;
   end Calculate_Next_AOS_LOS;

   ----------------------
   -- Compare_Elements --
   ----------------------

   function Compare_Elements
     (Left			: in Satellite_Data;
      Right			: in Satellite_Data) return Boolean
   is
   begin
      return Left = Right;
   end Compare_Elements;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This			:    out Data;
      Groundstation_Name	: in     String;
      Latitude			: in     Long_Float;
      Longitude			: in     Long_Float;
      Height			: in     Long_Float)
   is
      Success			: Boolean;

      procedure Iterate_Proc (Cursor : Satellite_Vector.Cursor) is
         Item	: Satellite_Data := Element (Cursor);
      begin
         Item.Next_Check := Null_Time;
         Replace_Element (This.Satellites, Cursor, Item);
      end Iterate_Proc;

   begin
      if not Colour_Configured then
         Groundstation_Colour := Gdk.Color.Parse ("Yellow");
         Alloc_Color (Get_Default_Colormap, Groundstation_Colour, False, True, Success);
         Inrange_Colour := Gdk.Color.Parse ("Green");
         Alloc_Color (Get_Default_Colormap, Inrange_Colour, False, True, Success);
         Colour_Configured := True;
      end if;

      EarthStation.Predict.Initialize_Observer
        (This		=> This.Groundstation,
         Latitude	=> Latitude,
         Longitude	=> Longitude,
         Height		=> Height);

      This.Latitude := Latitude;
      This.Longitude := Longitude;
      Set_Unbounded_String (This.Groundstation_Name, Groundstation_Name);

      --  Force a recalculation (in case groundstation has moved a long way)
      Iterate (This.Satellites, Iterate_Proc'Access);
   end Initialize;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (This			: in out Data;
      Map_Display		: access Map_Display_Record'Class;
      Table_Display		: access Data_Table_Record'Class)
   is
      Dn			: Long_Integer := 0;
      Df			: Long_Float := 0.0;
      Now			: constant Time := Clock;
      Update_Sat		: constant Boolean := Now /= This.Last_Update;

      package ESDT renames EarthStation.Data_Table;
      package ESP renames EarthStation.Predict;

      procedure Iterate_Proc (Cursor : Satellite_Vector.Cursor) is
         Item	: Satellite_Data := Element (Cursor);
      begin
         if Update_Sat then
            Item.Sat_Vectors := ESP.Calculate_Satellite_Vectors
              (Item.Satellite, Dn, Df);
            Item.Rng_Vectors := ESP.Calculate_Range_Vectors
              (This.Groundstation, Item.Sat_Vectors);
            if Now > Item.Next_Check then
               Calculate_Next_AOS_LOS (This, Item);
            end if;
            Replace_Element (This.Satellites, Cursor, Item);
         end if;

         if ESP.Get_Elevation (Item.Rng_Vectors) > Minimum_Elevation then
            EarthStation.Map_Display.Draw_Footprint
              (This		=> Map_Display,
               Id		=> To_String (Item.Satellite_Id),
               RS_Distance	=> ESP.Get_RS (Item.Sat_Vectors),
               Latitude		=> ESP.Get_Latitude (Item.Rng_Vectors),
               Longitude	=> ESP.Get_Longitude (Item.Rng_Vectors),
               Colour		=> Inrange_Colour);
         else
            EarthStation.Map_Display.Draw_Footprint
              (This		=> Map_Display,
               Id		=> To_String (Item.Satellite_Id),
               RS_Distance	=> ESP.Get_RS (Item.Sat_Vectors),
               Latitude		=> ESP.Get_Latitude (Item.Rng_Vectors),
               Longitude	=> ESP.Get_Longitude (Item.Rng_Vectors),
               Colour		=> Groundstation_Colour);
         end if;

         if To_Index (Cursor) = This.Selected_Satellite then
            ESDT.Set_Altitude (Table_Display, ESP.Get_Height (Item.Sat_Vectors));
            ESDT.Set_Azimuth (Table_Display, ESP.Get_Azimuth (Item.Rng_Vectors));
            ESDT.Set_Elevation (Table_Display, ESP.Get_Elevation (Item.Rng_Vectors));
            ESDT.Set_Latitude (Table_Display, ESP.Get_Latitude (Item.Rng_Vectors));
            ESDT.Set_Longitude (Table_Display, ESP.Get_Longitude (Item.Rng_Vectors));
            ESDT.Set_Max_Elevation (Table_Display, Item.Max_Elevation);
            ESDT.Set_Next_AOS (Table_Display, Item.Next_AOS);
            ESDT.Set_Next_LOS (Table_Display, Item.Next_LOS);
            ESDT.Set_Orbit (Table_Display, ESP.Get_Orbit (Item.Sat_Vectors));
            ESDT.Set_Range (Table_Display, ESP.Get_Range (Item.Rng_Vectors));
            ESDT.Set_Range_Rate (Table_Display, ESP.Get_Range_Rate (Item.Rng_Vectors));
            ESDT.Set_Satellite_Id (Table_Display, To_String (Item.Satellite_Id));

            case ESP.Get_Visibility (Item.Sat_Vectors) is
               when ESP.ECLIPSED	=> ESDT.Set_Visibility (Table_Display, "Ecl");
               when ESP.SHADE_SIDE	=> ESDT.Set_Visibility (Table_Display, "-");
               when ESP.SUN_SIDE	=> ESDT.Set_Visibility (Table_Display, "+");
               when ESP.VISIBLE	=> ESDT.Set_Visibility (Table_Display, "Vis");
            end case;
         end if;
      end Iterate_Proc;

   begin
      EarthStation.Map_Display.Draw_Point
        (This		=> Map_Display,
         Id		=> To_String (This.Groundstation_Name),
         Latitude	=> This.Latitude,
         Longitude	=> This.Longitude,
         Colour		=> Groundstation_Colour);

      EarthStation.Predict.Clock_To_Day (Now, Dn, Df);
      Iterate (This.Satellites, Iterate_Proc'Access);

      This.Last_Update := Now;
   end Update_Display;

end EarthStation.Tracking;

