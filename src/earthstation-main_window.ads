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

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Config_File;			use Config_File;
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
      Edit_Menu		: Gtk_Menu;
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

   procedure Handle_Clear_Keplerian_Elements
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window);
   --  Clears Keplerian Elements directory.

   function Handle_Clear_Keplerian_Iter
     (Satellite_Name	: in String) return Boolean;
   --  Iterator used to delete Satellite Keplerian Elements.

   procedure Handle_Groundstation_Menu_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window);
   --  Handles Groundstation Menu selections

   procedure Handle_Import_TLE
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window);
   --  Handles Import TLE selection from main menu.
   --  Asks user for a TLE file and imports Keplerian Elements from that
   --  file.

   procedure Handle_Import_TLE_OK (Filename : in String);

   function Handle_Timeout (This : in Main_Window) return Boolean;
   --  Timer tick handler (updates display)

   procedure Handle_Track_Menu_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     EarthStation.Tracking.Data_Access);
   --  Handles Active Track Menu selections

   procedure Handle_Tracking_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window);
   --  Handles Tracking Select Dialogue.

   procedure Initialize_Directories;
   --  Creates the Keplerian Elements and Preferences Directories (if needed)

   procedure Load_Preferences (Config : in out Config_File.Config_Data);
   --  Loads preferences from disk

   function Looks_Like_TLE
     (Name		: in Unbounded_String;
      Line_1		: in Unbounded_String;
      Line_2		: in Unbounded_String) return Boolean;
   --  Returns true if the given lines look like a TLE element

   procedure Save_Preferences (Config : in out Config_File.Config_Data);
   --  Saves preferences to disk

   procedure Show_About_Box (Object : access Gtk_Menu_Item_Record'Class);
   --  Shows the About Box dialogue box

   procedure Update_Tracking_Menu (This : access Main_Window_Record'Class);
   --  Updates the Tracking Menu

end EarthStation.Main_Window;

