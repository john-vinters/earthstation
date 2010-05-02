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

with Gtk;				use Gtk;
with Gtk.Enums;				use Gtk.Enums;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Main;				use Gtk.Main;

package body EarthStation.Main_Window is

   package Window_Callback is new Handlers.Callback (Gtk_Widget_Record);

   ---------------
   -- Exit_Main --
   ---------------

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Object);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Main_Window_Record'Class) is
   begin
      Gtk.Window.Initialize (This, Window_Toplevel);
      Set_Title (This, "EarthStation");
      Set_Default_Size (This, 800, 600);
      Set_Position (This, Win_Pos_Center);

      Window_Callback.Connect
        (This, "destroy", Window_Callback.To_Marshaller (Exit_Main'Access));

      Gtk_New_VBox (This.VBox, Homogeneous => False, Spacing => 1);
      Add (This, This.VBox);
   end Initialize;

end EarthStation.Main_Window;

