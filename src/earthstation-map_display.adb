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
--  earthstation-map_display.adb	jvinters	2-May-2010
--

pragma License (GPL);

with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Ada.Numerics;			use Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
					use Ada.Numerics.Long_Elementary_Functions;
with EarthStation.Predict;		use EarthStation.Predict;
with Gdk;				use Gdk;
with Gdk.Drawable;			use Gdk.Drawable;
with Gdk.Font;				use Gdk.Font;
with Gdk.GC;				use Gdk.GC;
with Gdk.RGB;				use Gdk.RGB;
with Glib.Error;			use Glib.Error;
with Gtk;				use Gtk;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Style;				use Gtk.Style;
with Gtk.Widget;			use Gtk.Widget;

package body EarthStation.Map_Display is

   package Expose_Callback is new Handlers.Return_Callback
     (Map_Display_Record, Boolean);

   Blue		: Gdk_Color := Gdk.Color.Parse ("Blue");
   Cyan		: Gdk_Color := Gdk.Color.Parse ("Cyan");
   Green	: Gdk_Color := Gdk.Color.Parse ("Green");
   Red		: Gdk_Color := Gdk.Color.Parse ("Red");
   White	: Gdk_Color := Gdk.Color.Parse ("White");
   Yellow	: Gdk_Color := Gdk.Color.Parse ("Yellow");

   Clock_Font	: Gdk.Font.Gdk_Font;
   Grid_Font	: Gdk.Font.Gdk_Font;
   Id_Font	: Gdk.Font.Gdk_Font;

   ----------------
   -- Draw_Clock --
   ----------------

   procedure Draw_Clock
     (This		: access Map_Display_Record'Class;
      UTC		: in Time)
   is
      GC		: Gdk.GC.Gdk_GC;
      Time_Str		: constant String := "UTC: " & Image (UTC) & 'Z';
      Time_Str_Ht	: constant Gint := String_Height (Clock_Font, Time_Str);
      Time_Str_Len	: constant Gint := String_Width (Clock_Font, Time_Str);
      X			: constant Gint := This.Current_Width - Time_Str_Len;
      Y			: constant Gint := Time_Str_Ht;
   begin
      Gdk_New (GC, This.Screen_Image);
      Set_Foreground (GC, Blue);
      Draw_Rectangle
        (This.Screen_Image, GC, True, X - 2, 0, Time_Str_Len + 2, Time_Str_Ht + 2);
      Set_Foreground (GC, White);
      Draw_Text (This.Screen_Image, Clock_Font, GC, X, Y, Time_Str);
      Unref (GC);
   end Draw_Clock;

   --------------------
   -- Draw_Footprint --
   --------------------

   procedure Draw_Footprint
     (This		: access Map_Display_Record'Class;
      Id		: in  String;
      RS_Distance	: in  Long_Float;
      Latitude		: in  Long_Float;
      Longitude		: in  Long_Float;
      Colour		: in  Gdk.Color.Gdk_Color)
   is
      Cos_Lat		: constant Long_Float := Cos (Radians (Latitude));
      Cos_Lon		: constant Long_Float := Cos (Radians (Longitude));
      Cos_Radius	: Long_Float;
      GC		: Gdk.GC.Gdk_GC;
      Lat		: Long_Float;
      Lon		: Long_Float;
      Nx, Ny		: Gint;
      Px, Py		: Gint := -9999;
      Radius		: constant Long_Float := Arccos (Predict.RE / RS_Distance);
      Sin_Lat		: constant Long_Float := Sin (Radians (Latitude));
      Sin_Lon		: constant Long_Float := Sin (Radians (Longitude));
      Sin_Radius	: Long_Float;
      X, Y, Z		: Long_Float;
      X2, Y2, Z2	: Long_Float;
      W2		: constant Gint := This.Current_Width / 2;
   begin
      Gdk_New (GC, This.Screen_Image);
      Set_Foreground (GC, Colour);
      Set_Line_Attributes (GC, 3, Line_Solid, Cap_Round, Join_Miter);
      Cos_Radius := Cos (Radius);
      Sin_Radius := Sin (Radius);

      for i in 0 .. Footprint_Steps loop
         X := Cos_Radius;
         Y := Sin_Radius * Sin_Table (i);
         Z := Sin_Radius * Cos_Table (i);

         X2 := X * Cos_Lat - Z * Sin_Lat;
         Y2 := Y;
         Z2 := X * Sin_Lat + Z * Cos_Lat;

         X := X2 * Cos_Lon - Y2 * Sin_Lon;
         Y := X2 * Sin_Lon + Y2 * Cos_Lon;
         Z := Z2;

         Lon := Degrees (Predict.Atn (Y, X));
         Lat := Degrees (Arcsin (Z));
         Nx := Longitude_To_X (This, Lon);
         Ny := Latitude_To_Y (This, Lat);

         if Px /= -9999 then
            if abs (Nx - Px) < W2 then
               Draw_Line (This.Screen_Image, GC, Nx, Ny, Px, Py);
            else
               if Nx < W2 then
                  Draw_Line (This.Screen_Image, GC, Px, Py, This.Current_Width, Py);
                  Draw_Line (This.Screen_Image, GC, Nx, Ny, 0, Ny);
               elsif Nx > W2 then
                  Draw_Line (This.Screen_Image, GC, Px, Py, 0, Py);
                  Draw_Line (This.Screen_Image, GC, Nx, Ny, This.Current_Width, Ny);
               end if;
            end if;
         end if;

         Px := Nx;
         Py := Ny;
      end loop;

      Nx := Longitude_To_X (This, Longitude);
      Ny := Latitude_To_Y (This, Latitude);
      Set_Foreground (GC, Red);
      Draw_Text (This.Screen_Image, Id_Font, GC, Nx + 5, Ny - 5, Text => Id);
      Draw_Rectangle (This.Screen_Image, GC, True, Nx - 1, Ny - 1, 3, 3);

      Unref (GC);
   end Draw_Footprint;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid (This : access Map_Display_Record'Class) is
      CX	: constant Gint := Longitude_To_X (This, 0.0);
      CY        : constant Gint := Latitude_To_Y (This, 0.0);
      GC	: Gdk.GC.Gdk_GC;
      Height	: constant Gint := This.all.Current_Height;
      L		: Long_Float;
      Width	: constant Gint := This.all.Current_Width;
      X, Y	: Gint;
   begin
      Gdk_New (GC, This.Scaled_Map);
      Set_Background (GC, Blue);
      Set_Foreground (GC, Cyan);
      Set_Line_Attributes (GC, 1, Line_Double_Dash, Cap_Round, Join_Miter);

      --  Draw prime meridian and equator
      Draw_Line (This.Scaled_Map, GC, 0, Height / 2, Width, Height / 2);
      Draw_Line (This.Scaled_Map, GC, Width / 2, 0, Width / 2, Height);

      --  Draw Latitude Grid Lines
      Set_Line_Attributes (GC, 1, Line_On_Off_Dash, Cap_Round, Join_Miter);
      for Latitude in -3 .. 3 loop
         L := Long_Float (Latitude) * 30.0;
         Y := Latitude_To_Y (This, L);

         if Latitude /= 0 then
            Set_Foreground (GC, Blue);
            Draw_Line (This.Scaled_Map, GC, 0, Y, Width, Y);
         end if;

         Set_Foreground (GC, Red);
         if Latitude < 0 then
            declare
               Deg_South	: constant Integer := abs (Integer (L));
               South_Str	: constant String := Deg_South'Img & "'S";
            begin
               Draw_Text (This.Scaled_Map, Grid_Font, GC, CX, Y - 2, South_Str);
            end;
         elsif Latitude > 0 then
            declare
               Deg_North	: constant Integer := Integer (L);
               North_Str	: constant String := Deg_North'Img & "'N";
            begin
               Draw_Text (This.Scaled_Map, Grid_Font, GC, CX, Y - 2, North_Str);
            end;
         else
            declare
               Label		: constant String := " 0'";
            begin
               Draw_Text (This.Scaled_Map, Grid_Font, GC, CX, Y - 2, Label);
            end;
         end if;
      end loop;

      --  Draw Longitude Grid Lines
      for Longitude in -6 .. 6 loop
         L := Long_Float (Longitude) * 30.0;
         X := Longitude_To_X (This, L);

         if Longitude /= 0 then
            Set_Foreground (GC, Blue);
            Draw_Line (This.Scaled_Map, GC, X, 0, X, Height);
         end if;

         Set_Foreground (GC, Red);
         if Longitude < 0 then
            declare
               Deg_West		: constant Integer := abs (Integer (L));
               West_Str		: constant String := Deg_West'Img & "'W";
            begin
               Draw_Text (This.Scaled_Map, Grid_Font, GC, X, CY - 2, West_Str);
            end;
         elsif Longitude > 0 then
            declare
               Deg_East		: constant Integer := Integer (L);
               East_Str		: constant String := Deg_East'Img & "'E";
            begin
               Draw_Text (This.Scaled_Map, Grid_Font, GC, X, CY - 2 , East_Str);
            end;
         end if;
      end loop;

      Unref (GC);
   end Draw_Grid;

   ----------------
   -- Draw_Point --
   ----------------

   procedure Draw_Point
     (This		: access Map_Display_Record'Class;
      Id		: in  String;
      Latitude		: in  Long_Float;
      Longitude		: in  Long_Float;
      Colour		: in  Gdk.Color.Gdk_Color)
   is
      GC		: Gdk.GC.Gdk_GC;
      Px, Py		: Gint;
   begin
      Gdk_New (GC, This.Scaled_Map);
      Set_Foreground (GC, Colour);

      Px := Longitude_To_X (This, Longitude);
      Py := Latitude_To_Y (This, Latitude);

      Draw_Rectangle (This.Scaled_Map, GC, True, Px - 1, Py - 1, 3, 3);
      if Id'Length > 0 then
         Draw_Text (This.Scaled_Map, Id_Font, GC, Px + 5, Py, Id);
      end if;

      Unref (GC);
   end Draw_Point;

   ------------
   -- Expose --
   ------------

   function Expose (This : access Map_Display_Record'Class) return Boolean is
      Drawable	: constant Gdk.Drawable.Gdk_Drawable := Get_Window (This.Area);
      GC	: constant Gdk.GC.Gdk_GC := Get_Black_GC (Get_Style (This));
      Height	: constant Gint := Get_Allocation_Height (This.Area);
      Width	: constant Gint := Get_Allocation_Width (This.Area);
   begin
      --  we cache the map bitmap so that it can be quickly output.
      --  only resize the bitmap if we actually need to...

      if Width /= This.Current_Width or else Height /= This.Current_Height then
         Resize_Map (This, Width, Height);
      end if;

      Draw_Drawable (Drawable, GC, This.Screen_Image, 0, 0, 0, 0, Width, Height);
      return False;
   end Expose;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (This		: out Map_Display;
      Map_Filename	: in  String)
   is
   begin
      This := new Map_Display_Record;
      Initialize (This, Map_Filename);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This		: access Map_Display_Record'Class;
      Map_Filename	: in  String)
   is
      Err		: GError;
      Success		: Boolean;
   begin
      Initialize_VBox (This, Homogeneous => False, Spacing => 0);
      Gtk_New (This.Area);
      Pack_Start (This, This.Area);
      Gdk.Pixbuf.Gdk_New_From_File (This.Map_Image, Map_Filename, Err);

      Alloc_Color (Get_Default_Colormap, Blue, False, True, Success);
      Alloc_Color (Get_Default_Colormap, Cyan, False, True, Success);
      Alloc_Color (Get_Default_Colormap, Green, False, True, Success);
      Alloc_Color (Get_Default_Colormap, Red, False, True, Success);
      Alloc_Color (Get_Default_Colormap, Yellow, False, True, Success);
      Alloc_Color (Get_Default_Colormap, White, False, True, Success);

      Load (Clock_Font, "-*-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*");
      Load (Grid_Font, "-*-lucidatypewriter-medium-r-*-*-9-*-*-*-*-*-*-*");
      Load (Id_Font, "-*-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*");

      Expose_Callback.Object_Connect
        (This.Area, "expose_event", Expose_Callback.To_Marshaller (Expose'Access), This);

      Show (This);
   end Initialize;

   -------------------
   -- Latitude_To_Y --
   -------------------

   function Latitude_To_Y
     (This		: access Map_Display_Record'Class;
      Latitude		: in  Long_Float) return Gint
   is
      Half_Height	: constant Long_Float := Long_Float (This.Current_Height) / 2.0;
   begin
      return Gint (Half_Height - (Latitude * (Half_Height / 90.0)));
   end Latitude_To_Y;

   --------------------
   -- Longitude_To_X --
   --------------------

   function Longitude_To_X
     (This		: access Map_Display_Record'Class;
      Longitude		: in  Long_Float) return Gint
   is
      Adj_Longitude	: Long_Float := Longitude;
      Half_Width	: constant Long_Float := Long_Float (This.Current_Width) / 2.0;
   begin
      while Adj_Longitude > 180.0 loop
         Adj_Longitude := Adj_Longitude - 360.0;
      end loop;

      while Adj_Longitude < -180.0 loop
         Adj_Longitude := Adj_Longitude + 360.0;
      end loop;

      return Gint (Half_Width + (Adj_Longitude * (Half_Width / 180.0)));
   end Longitude_To_X;

   ----------------
   -- Resize_Map --
   ----------------

   procedure Resize_Map
     (This		: access Map_Display_Record'Class;
      Width		: in  Gint;
      Height		: in  Gint)
   is
      GC		: constant Gdk.GC.Gdk_GC := Get_Black_GC (Get_Style (This));
      Temp		: constant Gdk_Pixbuf := Scale_Simple (This.Map_Image, Width, Height);
   begin
      This.Current_Height := Height;
      This.Current_Width := Width;

      if This.Scaled_Map /= Null_Pixmap then
         Gdk.Pixmap.Unref (This.Scaled_Map);
      end if;

      Gdk_New (This.Scaled_Map, Get_Window (This.Area), Width, Height);
      Render_To_Drawable
        (Temp, This.Scaled_Map, GC, 0, 0, 0, 0, Width, Height, Dither_Normal, 0, 0);
      Unref (Temp);

      if This.Draw_Grid then
         Draw_Grid (This);
      end if;

      if This.Screen_Image /= Null_Pixmap then
         Gdk.Pixmap.Unref (This.Screen_Image);
      end if;

      Gdk_New (This.Screen_Image, Get_Window (This.Area), Width, Height);
      Update_Start (This);
   end Resize_Map;

   ----------------
   -- Update_End --
   ----------------

   procedure Update_End (This : access Map_Display_Record'Class) is
      Now		: constant Time := Clock;
      Junk		: Boolean;
   begin
      Draw_Clock (This, Now);
      Junk := Expose (This);
      pragma Unreferenced (Junk);
   end Update_End;

   ------------------
   -- Update_Start --
   ------------------

   procedure Update_Start (This : access Map_Display_Record'Class) is
      GC		: constant Gdk.GC.Gdk_GC := Get_Black_GC (Get_Style (This));
      H			: constant Gint := This.all.Current_Height;
      W			: constant Gint := This.all.Current_Width;
   begin
      Draw_Drawable (This.Screen_Image, GC, This.Scaled_Map, 0, 0, 0, 0, W, H);
   end Update_Start;

begin
   --  Precompute the Sin and Cos tables used for drawing satellite footprints.
   --  We can do this because they use the same values repeatedly.

   declare
      Angle	: Long_Float;
   begin
      for i in 0 .. Footprint_Steps loop
         Angle := 2.0 * Pi * Long_Float (i) / Long_Float (Footprint_Steps);
         Sin_Table (i) := Sin (Angle);
         Cos_Table (i) := Cos (Angle);
      end loop;
   end;   
end EarthStation.Map_Display;

