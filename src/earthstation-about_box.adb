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
--  earthstation-about_box.adb	jvinters	3-May-2010
--

pragma License (GPL);

with Gtk.About_Dialog;			use Gtk.About_Dialog;
with Gtk.Dialog;			use Gtk.Dialog;
with Gtk.Enums;				use Gtk.Enums;

package body EarthStation.About_Box is

   LF		: constant Character := ASCII.LF;

   ---------
   -- Run --
   ---------

   procedure Run is
      Dialog	: Gtk_About_Dialog;
   begin
      Gtk_New (Dialog);
      Set_Position (Dialog, Win_Pos_Center);
      Set_Destroy_With_Parent (Dialog, True);
      Set_Modal (Dialog, True);
      Set_Comments (Dialog, "Satellite Tracker");
      Set_Copyright (Dialog, "Copyright (c) 2010, John Vinters G7NSN");
      Set_Name (Dialog, "EarthStation");
      Set_Version (Dialog, "0.10");
      Set_Website (Dialog, "http://github.com/john-vinters/earthstation");
      Set_License (Dialog,
        "This program is free software; you can redistribute it and/or" & LF
        & "modify it under the terms of the GNU General Public" & LF
        & "License as published by the Free Software Foundation; either" & LF
        & "version 3 of the License, or (at your option) any later version." & LF
        & LF
        & "This program is distributed in the hope that it will be useful," & LF
        & "but WITHOUT ANY WARRANTY; without even the implied warranty of" & LF
        & "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." & LF
        & "See the GNU General Public License for more details.");

      if Run (Dialog) /= Gtk_Response_Close then
         null;		--  ignore result
      end if;

      Destroy (Dialog);
   end Run;

end EarthStation.About_Box;

