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
--  earthstation-main_window.ads	jvinters	2-May-2010
--

pragma License (GPL);

with EarthStation.Data_Table;		use EarthStation.Data_Table;
with EarthStation.Map_Display;		use EarthStation.Map_Display;
with EarthStation.Tracking;		use EarthStation.Tracking;
with Gtk.Box;				use Gtk.Box;
with Gtk.Menu;				use Gtk.Menu;
with Gtk.Menu_Bar;			use Gtk.Menu_Bar;
with Gtk.Menu_Item;			use Gtk.Menu_Item;
with Gtk.Status_Bar;			use Gtk.Status_Bar;
with Gtk.Widget;			use Gtk.Widget;
with Gtk.Window;			use Gtk.Window;

package EarthStation.Main_Window is

   type Main_Window_Record is new Gtk_Window_Record with private;
   type Main_Window is access all Main_Window_Record'Class;

   procedure Gtk_New (This : out Main_Window);

   procedure Initialize (This : access Main_Window_Record'Class);

private

   type Main_Window_Record is new Gtk_Window_Record with record
      Active_Track	: Gtk_Menu_Item;
      Active_Track_Menu	: Gtk_Menu;
      Data		: aliased EarthStation.Tracking.Data;
      File_Menu		: Gtk_Menu;
      Help_Menu		: Gtk_Menu;
      Map		: Map_Display.Map_Display;
      Menu_Bar		: Gtk_Menu_Bar;
      Satellite_Data	: Data_Table.Data_Table;
      Status_Bar	: Gtk_Status_Bar;
      VBox		: Gtk_Box;
      View_Menu		: Gtk_Menu;
   end record;

   procedure Exit_Main (Object : access Gtk_Menu_Item_Record'Class);
   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);
   --  "destroy" event handler for when main window is closed by user

   function Handle_Timeout (This : in Main_Window) return Boolean;
   --  Timer tick handler (updates display)

   procedure Handle_Track_Menu_Select (Object : access Gtk_Menu_Item_Record'Class);
   --  Handles Active Track Menu selections

   procedure Show_About_Box (Object : access Gtk_Menu_Item_Record'Class);
   --  Shows the About Box dialogue box

end EarthStation.Main_Window;

