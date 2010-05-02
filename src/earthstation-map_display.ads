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
--  earthstation-map_display.ads	jvinters	2-May-2010
--

pragma License (GPL);

with Gdk.Color;				use Gdk.Color;
with Gdk.Pixbuf;			use Gdk.Pixbuf;
with Gdk.Pixmap;			use Gdk.Pixmap;
with Glib;				use Glib;
with Gtk.Box;				use Gtk.Box;
with Gtk.Drawing_Area;			use Gtk.Drawing_Area;

package EarthStation.Map_Display is

   type Map_Display_Record is new Gtk_Box_Record with private;
   type Map_Display is access all Map_Display_Record'Class;

   procedure Draw_Footprint
     (This		: access Map_Display_Record'Class;
      Id		: in  String;
      RS_Distance	: in  Long_Float;
      Latitude		: in  Long_Float;
      Longitude		: in  Long_Float;
      Colour		: in  Gdk.Color.Gdk_Color);
   --  Draws Satellite Footprint in the given colour.  This should be called
   --  inbetween calls to Update_Start and Update_End.

   procedure Draw_Point
     (This		: access Map_Display_Record'Class;
      Id		: in  String;
      Latitude		: in  Long_Float;
      Longitude		: in  Long_Float;
      Colour		: in  Gdk.Color.Gdk_Color);
   --  Draws a point of interest in the given colour.  This should be called
   --  inbetween calls to Update_Start and Update_End.

   function Expose (This : access Map_Display_Record'Class) return Boolean;

   procedure Gtk_New
     (This		: out Map_Display;
      Map_Filename	: in  String);

   procedure Initialize
     (This		: access Map_Display_Record'Class;
      Map_Filename	: in  String);

   procedure Update_End (This : access Map_Display_Record'Class);
   --  Called once drawing on map has been completed (triggers an expose event)

   procedure Update_Start (This : access Map_Display_Record'Class);
   --  Called before satellite tracks etc. can be drawn (this draws the map
   --  layer, which can then be overlaid onto).

private

   type Map_Display_Record is new Gtk_Box_Record with record
      Area		: Gtk_Drawing_Area;
      Current_Height    : Gint := -1;
      Current_Width	: Gint := -1;
      Draw_Grid		: Boolean := True;
      Map_Image		: Gdk_Pixbuf;
      Scaled_Map	: Gdk_Pixmap;
      Screen_Image	: Gdk_Pixmap;
   end record;

   Footprint_Steps	: constant := 270;

   type Trig_Table is array (Natural range <>) of Long_Float;

   Cos_Table		: Trig_Table (0 .. Footprint_Steps);
   Sin_Table		: Trig_Table (0 .. Footprint_Steps);

   procedure Draw_Grid (This : access Map_Display_Record'Class);
   --  Overlays the Latitude/Longitude grid

   function Latitude_To_Y
     (This		: access Map_Display_Record'Class;
      Latitude		: in  Long_Float) return Gint;
   --  Converts Latitude to Y Coordinate on the map display

   function Longitude_To_X
     (This		: access Map_Display_Record'Class;
      Longitude		: in  Long_Float) return Gint;
   --  Converts Longitude to X Coordinate on the map display, wrapping the
   --  longitude so that it is in the range -180.0 .. 180.0

   procedure Resize_Map
     (This		: access Map_Display_Record'Class;
      Width		: in  Gint;
      Height		: in  Gint);
   --  Resizes internal map bitmap to the specified height and width

end EarthStation.Map_Display;

