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
--  earthstation-select_satellite.adb	jvinters	31-May-2010
--

pragma License (GPL);

with EarthStation.Keplerian_Elements;	use EarthStation.Keplerian_Elements;
with EarthStation.Predict;		use EarthStation.Predict;
with Glib;				use Glib;
with Glib.Object;			use Glib.Object;
with Glib.Values;			use Glib.Values;
with Gtk.Cell_Renderer_Text;		use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;		use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;				use Gtk.Enums;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Scrolled_Window;		use Gtk.Scrolled_Window;
with Gtk.Table;				use Gtk.Table;
with Gtk.Tree_Model;			use Gtk.Tree_Model;
with Gtk.Tree_View_Column;		use Gtk.Tree_View_Column;
with Gtk.Widget;			use Gtk.Widget;

package body EarthStation.Select_Satellite is

   package Object_Callback is new Gtk.Handlers.Callback (GObject_Record);

   Active_Column	: constant := 1;
   Text_Column		: constant := 0;

   Temp_Store		: Gtk.Tree_Store.Gtk_Tree_Store;
   Temp_Track		: EarthStation.Tracking.Data_Access;

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Model		: access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Text		: in String;
      Active		: in Boolean := False)
     return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter		: Gtk.Tree_Model.Gtk_Tree_Iter := Get_Iter_First (Model);
      New_Iter		: Gtk.Tree_Model.Gtk_Tree_Iter := Null_Iter;
   begin
      while Iter /= Null_Iter loop
         declare
            Str		: constant String := Get_String (Model, Iter, Text_Column);
         begin
            if Text < Str then
               Insert_Before (Model, New_Iter, Null_Iter, Iter);
               Set (Model, New_Iter, Text_Column, Text);
               Set (Model, New_Iter, Active_Column, Active);
               return New_Iter;
            else
               Next (Model, Iter);
            end if;
         end;
      end loop;

      Append (Model, New_Iter, Null_Iter);
      Set (Model, New_Iter, Text_Column, Text);
      Set (Model, New_Iter, Active_Column, Active);
      return New_Iter;
   end Add_Line;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Model		: access GObject_Record'Class;
      Params		: in Glib.Values.GValues)
   is
      Store		: constant Gtk_Tree_Store := Gtk_Tree_Store (Model);
      Path		: constant String := Get_String (Nth (Params, 1));
      Iter		: constant Gtk_Tree_Iter := 
                            Get_Iter_From_String (Store, Path);
      Old_Value		: Boolean;
   begin
      Old_Value := Get_Boolean (Store, Iter, Active_Column);
      Set (Store, Iter, Active_Column, not Old_Value);
   end Edited_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (This		:    out Select_Satellite;
      Tracking		: in     EarthStation.Tracking.Data_Access)
   is
   begin
      This := new Select_Satellite_Record;
      EarthStation.Select_Satellite.Initialize (This, Tracking);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This		: access Select_Satellite_Record'Class;
      Tracking		: in     EarthStation.Tracking.Data_Access)
   is
      Num		: Gint;
      Sat_Column	: Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Sel_Column	: Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Scrolled		: Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Table		: Gtk_Table;
      Text_Render	: Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Toggle_Render	: Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
   begin
      Gtk.Dialog.Initialize (This);
      Set_Border_Width (This, 0);
      Set_Title (This, "Select Satellite(s)");
      Set_Size_Request (This, 225, 300);

      Gtk_New (Table, Rows => 1, Columns => 1, Homogeneous => False);
      Set_Border_Width (Table, 4);

      Gtk_New (This.all.Store,
        (Text_Column	=> GType_String,
         Active_Column	=> GType_Boolean));

      Gtk_New (This.all.View, This.all.Store);
      Gtk_New (Text_Render);
      Gtk_New (Toggle_Render);

      Gtk_New (Sat_Column);
      Num := Append_Column (This.all.View, Sat_Column);
      pragma Unreferenced (Num);
      Set_Sort_Column_Id (Sat_Column, Text_Column);
      Set_Title (Sat_Column, "Satellite");
      Pack_Start (Sat_Column, Text_Render, Expand => True);
      Set_Sizing (Sat_Column, Tree_View_Column_Autosize);
      Add_Attribute (Sat_Column, Text_Render, "text", Text_Column);

      Gtk_New (Sel_Column);
      Num := Append_Column (This.all.View, Sel_Column);
      Set_Title (Sel_Column, "Track");
      Pack_Start (Sel_Column, Toggle_Render, Expand => False);
      Add_Attribute (Sel_Column, Toggle_Render, "active", Active_Column);

      Set_Headers_Clickable (This.all.View, True);

      Object_Callback.Object_Connect
        (Toggle_Render, "toggled", Edited_Callback'Access, 
         Slot_Object => This.all.Store);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Scrolled, This.all.View);
      Show_All (Scrolled);

      Attach (Table, Scrolled, 0, 1, 0, 1);

      Temp_Store := This.all.Store;
      Temp_Track := Tracking;
      Iterate_Satellite_Names (Satellite_Iter'Access);
      Temp_Track := null;
      Temp_Store := null;

      This.Get_VBox.Pack_Start (Table, Expand => True, Fill => True);
      Gtk_New (This.all.OK_Button, "OK");
      Set_Flags (This.all.OK_Button, Can_Default);
      Add_Action_Widget (This, This.all.OK_Button, Gtk_Response_OK);

      Show_All (This);
   end Initialize;

   --------------------
   -- Satellite_Iter --
   --------------------

   function Satellite_Iter (Satellite_Name : in String) return Boolean is
      Iter	: Gtk_Tree_Iter;
   begin
      if Is_Tracked (Temp_Track.all, Satellite_Name) then
         Iter := Add_Line (Temp_Store, Satellite_Name, Active => True);
      else
         Iter := Add_Line (Temp_Store, Satellite_Name, Active => False);
      end if;
      pragma Unreferenced (Iter);
      return True;
   end Satellite_Iter;

   --------------------------
   -- Update_Tracking_List --
   --------------------------

   procedure Update_Tracking_List
     (This		: access Select_Satellite_Record'Class;
      Tracking		: in out EarthStation.Tracking.Data)
   is
      Iter		: Gtk_Tree_Iter := Null_Iter;
   begin
      Clear (Tracking);
      Iter := Get_Iter_First (This.Store);
      while Iter /= Null_Iter loop
         if Get_Boolean (This.Store, Iter, Active_Column) then
            declare
               Id	: constant String := 
                            Get_String (This.Store, Iter, Text_Column);
               Kep	: EarthStation.Predict.Keplerian_Elements;
            begin
               Kep := EarthStation.Keplerian_Elements.Load (Id);
               EarthStation.Tracking.Add_Satellite (Tracking, Id, Kep);
            end;
         end if;
         Next (This.Store, Iter);
      end loop;
   end Update_Tracking_List;

end EarthStation.Select_Satellite;

