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
--  es.adb	jvinters	2-May-2010
--

pragma License (GPL);

with EarthStation.Main_Window;		use EarthStation.Main_Window;
with Gtk.Main;				use Gtk.Main;

procedure ES is
   Main_Win	: Main_Window;
begin
   Gtk.Main.Init;
   Gtk_New (Main_Win);
   Show_All (Main_Win);
   Gtk.Main.Main;
end ES;

