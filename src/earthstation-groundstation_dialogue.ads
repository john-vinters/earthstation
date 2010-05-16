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
--  earthstation-groundstation_dialogue.ads	jvinters	15-May-2010
--

pragma License (GPL);

with Gtk.Dialog;			use Gtk.Dialog;
with Gtk.GEntry;			use Gtk.GEntry;

package EarthStation.Groundstation_Dialogue is

   type Groundstation_Dialogue_Record is new Gtk_Dialog_Record with private;
   type Groundstation_Dialogue is access all Groundstation_Dialogue_Record'Class;

   function Get_GS_Name
     (This 		: access Groundstation_Dialogue_Record'Class) return String;
   --  Returns the Groundstation Name

   function Get_Height
     (This		: access Groundstation_Dialogue_Record'Class) return Long_Float;
   --  Returns the Groundstation Height in metres.  May raise CONSTRAINT_ERROR
   --  if the setting in the dialogue box is invalid.

   function Get_Latitude
     (This		: access Groundstation_Dialogue_Record'Class) return Long_Float;
   --  Returns the Groundstation Latitude.  May raise CONSTRAINT_ERROR if
   --  the setting in the dialogue box is invalid.

   function Get_Longitude
     (This		: access Groundstation_Dialogue_Record'Class) return Long_Float;
   --  Returns the Groundstation Longitude.  May raise CONSTRAINT_ERROR if
   --  the setting in the dialogue box is invalid.

   procedure Gtk_New (This : out Groundstation_Dialogue);

   procedure Initialize (This : access Groundstation_Dialogue_Record'Class);

   procedure Set_GS_Name
     (This		: access Groundstation_Dialogue_Record'Class;
      Name		: in String);

   procedure Set_Height
     (This		: access Groundstation_Dialogue_Record'Class;
      Height		: in Long_Float);

   procedure Set_Latitude
     (This		: access Groundstation_Dialogue_Record'Class;
      Latitude		: in Long_Float);

   procedure Set_Longitude
     (This		: access Groundstation_Dialogue_Record'Class;
      Longitude		: in Long_Float);

private

   type Groundstation_Dialogue_Record is new Gtk_Dialog_Record with record
      Groundstation_Height		: Gtk_Entry;
      Groundstation_Latitude		: Gtk_Entry;
      Groundstation_Longitude		: Gtk_Entry;
      Groundstation_Name		: Gtk_Entry;
   end record;

end EarthStation.Groundstation_Dialogue;

