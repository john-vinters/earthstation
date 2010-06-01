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
--  earthstation-select_satellite.ads	jvinters	31-May-2010
--

pragma License (GPL);

with Gtk.Button;			use Gtk.Button;
with Gtk.Dialog;			use Gtk.Dialog;
with Gtk.Tree_Store;			use Gtk.Tree_Store;
with Gtk.Tree_View;			use Gtk.Tree_View;

package EarthStation.Select_Satellite is

   type Select_Satellite_Record is new Gtk.Dialog.Gtk_Dialog_Record with private;
   type Select_Satellite is access all Select_Satellite_Record'Class;

   procedure Gtk_New (This : out Select_Satellite);

   procedure Initialize (This : access Select_Satellite_Record'Class);

private

   type Select_Satellite_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Cancel_Button	: Gtk.Button.Gtk_Button;
      OK_Button		: Gtk.Button.Gtk_Button;
      Store		: Gtk.Tree_Store.Gtk_Tree_Store;
      View		: Gtk.Tree_View.Gtk_Tree_View;
   end record;

   function Satellite_Iter (Satellite_Name : in String) return Boolean;
   --  Iterates satellite names, adding them to the dialogue.

end EarthStation.Select_Satellite;

