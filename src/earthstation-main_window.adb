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
--  earthstation-main_window.adb	jvinters	2-May-2010
--

pragma License (GPL);

with Ada.Calendar;			use Ada.Calendar;
with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with EarthStation.About_Box;		use EarthStation.About_Box;
with Glib;				use Glib;
with Glib.Main;				use Glib.Main;
with Gtk;				use Gtk;
with Gtk.Enums;				use Gtk.Enums;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Main;				use Gtk.Main;

package body EarthStation.Main_Window is

   package Main_Window_Timeout is new Glib.Main.Generic_Sources (Main_Window);
   package Menu_Item_Callback is new Handlers.Callback (Gtk_Menu_Item_Record);
   package Window_Callback is new Handlers.Callback (Gtk_Widget_Record);

   ---------------
   -- Exit_Main --
   ---------------

   procedure Exit_Main (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      Main_Quit;
   end Exit_Main;

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Main_Quit;
   end Exit_Main;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (This : out Main_Window) is
   begin
      This := new Main_Window_Record;
      Initialize (This);
   end Gtk_New;

   --------------------
   -- Handle_Timeout --
   --------------------

   function Handle_Timeout (This : in Main_Window) return Boolean is
      Now		: constant Time := Clock;
   begin
      declare
         Junk		: Message_Id;
         Time_String	: constant String := "  UTC: " & Image (Now) & "Z";
      begin
         Pop (This.Status_Bar, 0);
         Junk := Push (This.Status_Bar, 0, Time_String);
         pragma Unreferenced (Junk);
      end;

      return True;
   end Handle_Timeout;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Main_Window_Record'Class) is
      Menu_Item		: Gtk_Menu_Item;
      Timeout		: G_Source_Id;
      pragma Unreferenced (Timeout);
   begin
      Gtk.Window.Initialize (This, Window_Toplevel);

      Gtk_New (This.Menu_Bar);
      Gtk_New (This.Map, "images/map.jpg");
      --  XXX FIXME: map filename be different when the app is installed! XXX

      Gtk_New (This.Satellite_Data);
      Gtk_New (This.Status_Bar);

      Set_Title (This, "EarthStation");
      Set_Default_Size (This, 800, 600);
      Set_Position (This, Win_Pos_Center);

      --  Setup main menu
      --  File Menu
      Gtk_New (This.File_Menu);
      Gtk_New_With_Mnemonic (Menu_Item, "E_xit");
      Menu_Item_Callback.Connect
        (Menu_Item, "activate", Menu_Item_Callback.To_Marshaller (Exit_Main'Access));
      Append (This.File_Menu, Menu_Item);
      Gtk_New_With_Mnemonic (Menu_Item, "_File");
      Append (This.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, This.File_Menu);

      --  Help Menu
      Gtk_New (This.Help_Menu);
      Gtk_New_With_Mnemonic (Menu_Item, "_About");
      Menu_Item_Callback.Connect
        (Menu_Item, "activate", Menu_Item_Callback.To_Marshaller (Show_About_Box'Access));
      Append (This.Help_Menu, Menu_Item);
      Gtk_New_With_Mnemonic (Menu_Item, "_Help");
      Append (This.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, This.Help_Menu);

      Window_Callback.Connect
        (This, "destroy", Window_Callback.To_Marshaller (Exit_Main'Access));

      Gtk_New_VBox (This.VBox, Homogeneous => False, Spacing => 1);
      Pack_Start (This.VBox, This.Menu_Bar, Expand => False, Fill => True);
      Pack_Start (This.VBox, This.Map, Expand => True, Fill => True);
      Pack_Start (This.VBox, This.Satellite_Data, Expand => False, Fill => True);
      Pack_Start (This.VBox, This.Status_Bar, Expand => False, Fill => True);

      Add (This, This.VBox);

      Timeout := Main_Window_Timeout.Timeout_Add
        (1000, Handle_Timeout'Access, Main_Window (This));
   end Initialize;

   --------------------
   -- Show_About_Box --
   --------------------

   procedure Show_About_Box (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      pragma Unreferenced (Object);
      EarthStation.About_Box.Run;
   end Show_About_Box;

end EarthStation.Main_Window;

