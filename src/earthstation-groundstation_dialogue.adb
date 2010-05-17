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
--  earthstation-groundstation_dialogue.adb	jvinters	15-May-2010
--

pragma License (GPL);

with Ada.Strings;			use Ada.Strings;
with Ada.Strings.Fixed;			use Ada.Strings.Fixed;
with Glib;				use Glib;
with Gtk.Button;			use Gtk.Button;
with Gtk.Enums;				use Gtk.Enums;
with Gtk.Label;				use Gtk.Label;
with Gtk.Table;				use Gtk.Table;
with Gtk.Widget;			use Gtk.Widget;

package body EarthStation.Groundstation_Dialogue is

   -----------------
   -- Get_GS_Name --
   -----------------

   function Get_GS_Name
     (This 	: access Groundstation_Dialogue_Record'Class) return String
   is
   begin
      return Get_Text (This.Groundstation_Name);
   end Get_GS_Name;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
     (This	: access Groundstation_Dialogue_Record'Class) return Long_Float
   is
   begin
      return Long_Float (Long_Integer'Value (Get_Text (This.Groundstation_Height)));
   end Get_Height;

   ------------------
   -- Get_Latitude --
   ------------------

   function Get_Latitude
     (This	: access Groundstation_Dialogue_Record'Class) return Long_Float
   is
   begin
      return Long_Float'Value (Get_Text (This.Groundstation_Latitude));
   end Get_Latitude;

   -------------------
   -- Get_Longitude --
   -------------------

   function Get_Longitude
     (This	: access Groundstation_Dialogue_Record'Class) return Long_Float
   is
   begin
      return Long_Float'Value (Get_Text (This.Groundstation_Longitude));
   end Get_Longitude;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (This : out Groundstation_Dialogue) is
   begin
      This := new Groundstation_Dialogue_Record;
      EarthStation.Groundstation_Dialogue.Initialize (This);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Groundstation_Dialogue_Record'Class) is
      Button		: Gtk_Button;
      Label		: Gtk_Label;
      Table		: Gtk_Table;
   begin
      Gtk.Dialog.Initialize (This);
      Set_Border_Width (This, 0);
      Set_Title (This, "Groundstation");
      -- Set_Size_Request (This, 225, 300);

      Gtk_New (Table, Rows => 4, Columns => 3, Homogeneous => False);
      Set_Border_Width (Table, 4);

      --  First Line, Name
      Gtk_New (Label, "Name: ");
      Set_Alignment (Label, 1.0, 0.5);
      Attach (Table, Label, 0, 1, 0, 1);

      Gtk_New (This.Groundstation_Name);
      Set_Max_Length (This.Groundstation_Name, 255);
      Set_Width_Chars (This.Groundstation_Name, 8);
      Attach (Table, This.Groundstation_Name, 1, 3, 0, 1);

      --  Second Line, Latitude
      Gtk_New (Label, "Latitude: ");
      Set_Alignment (Label, 1.0, 0.5);
      Attach (Table, Label, 0, 1, 1, 2);

      Gtk_New (This.Groundstation_Latitude);
      Set_Max_Length (This.Groundstation_Latitude, 8);
      Set_Width_Chars (This.Groundstation_Latitude, 5);
      Attach (Table, This.Groundstation_Latitude, 1, 2, 1, 2);

      Gtk_New (Label, " Deg North");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 2, 3, 1, 2);

      --  Third Line, Longitude
      Gtk_New (Label, "Longitude: ");
      Set_Alignment (Label, 1.0, 0.5);
      Attach (Table, Label, 0, 1, 2, 3);

      Gtk_New (This.Groundstation_Longitude);
      Set_Max_Length (This.Groundstation_Longitude, 8);
      Set_Width_Chars (This.Groundstation_Longitude, 5);
      Attach (Table, This.Groundstation_Longitude, 1, 2, 2, 3);

      Gtk_New (Label, " Deg East");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 2, 3, 2, 3);

      -- Fourth Line, Height
      Gtk_New (Label, "Height:");
      Set_Alignment (Label, 1.0, 0.5);
      Attach (Table, Label, 0, 1, 3, 4);

      Gtk_New (This.Groundstation_Height);
      Set_Max_Length (This.Groundstation_Height, 4);
      Set_Width_Chars (This.Groundstation_Height, 4);
      Attach (Table, This.Groundstation_Height, 1, 2, 3, 4);

      Gtk_New (Label, " Metres");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 2, 3, 3, 4);

      This.Get_VBox.Pack_Start (Table, Expand => True, Fill => True);

      Gtk_New (Button, "OK");
      Set_Flags (Button, Can_Default);
      Add_Action_Widget (This, Button, Gtk_Response_OK);

      Set_Position (This, Win_Pos_Center);
      Show_All (This);
   end Initialize;

   -----------------
   -- Set_GS_Name --
   -----------------

   procedure Set_GS_Name
     (This		: access Groundstation_Dialogue_Record'Class;
      Name		: in String)
   is
   begin
      Set_Text (This.Groundstation_Name, Name);
      Set_Position (This.Groundstation_Name, -1);
   end Set_GS_Name;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
     (This		: access Groundstation_Dialogue_Record'Class;
      Height		: in Long_Float)
   is
      HtStr		: constant String :=
        Long_Integer'Image (Long_Integer (Height));
   begin
      Set_Text (This.Groundstation_Height, Trim (HtStr, Both));
   end Set_Height;

   ------------------
   -- Set_Latitude --
   ------------------

   procedure Set_Latitude
     (This		: access Groundstation_Dialogue_Record'Class;
      Latitude		: in Long_Float)
   is
      type LR is delta 0.01 digits 8;
      CLat		: constant LR := LR (Latitude);
      CLatStr		: constant String := LR'Image (CLat);
   begin
      Set_Text (This.Groundstation_Latitude, Trim (CLatStr, Both));
   end Set_Latitude;

   -------------------
   -- Set_Longitude --
   -------------------

   procedure Set_Longitude
     (This		: access Groundstation_Dialogue_Record'Class;
      Longitude		: in Long_Float)
   is
      type LR is delta 0.01 digits 8;
      CLon		: constant LR := LR (Longitude);
      CLonStr		: constant String := LR'Image (CLon);
   begin
      Set_Text (This.Groundstation_Longitude, Trim (CLonStr, Both));
   end Set_Longitude;

end EarthStation.Groundstation_Dialogue;

