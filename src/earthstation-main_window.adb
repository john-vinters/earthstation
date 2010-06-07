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

with Ada.IO_Exceptions;
with Ada.Text_IO;			use Ada.Text_IO;
with EarthStation.About_Box;		use EarthStation.About_Box;
with EarthStation.Groundstation_Dialogue;
					use EarthStation.Groundstation_Dialogue;
with EarthStation.Keplerian_Elements;	use EarthStation.Keplerian_Elements;
with EarthStation.Platform;		use EarthStation.Platform;
with EarthStation.Predict;		use EarthStation.Predict;
with EarthStation.Select_Satellite;	use EarthStation.Select_Satellite;
with Glib;				use Glib;
with Glib.Main;				use Glib.Main;
with GNAT.OS_Lib;			use GNAT.OS_Lib;
with Gtk;				use Gtk;
with Gtk.Dialog;			use Gtk.Dialog;
with Gtk.Enums;				use Gtk.Enums;
with Gtk.Handlers;			use Gtk.Handlers;
with Gtk.Main;				use Gtk.Main;
with Gtkada.Dialogs;			use Gtkada.Dialogs;
with Gtkada.File_Selection;		use Gtkada.File_Selection;

package body EarthStation.Main_Window is

   package Main_Window_Timeout is new Glib.Main.Generic_Sources (Main_Window);
   package Menu_Item_Callback is new Handlers.Callback (Gtk_Menu_Item_Record);
   package Menu_Item_Window_Callback is new Handlers.User_Callback
     (Gtk_Menu_Item_Record, Main_Window);
   package Window_Callback is new Handlers.Callback (Gtk_Widget_Record);

   DEFAULT_GS_NAME		: constant String := "Halifax";
   DEFAULT_GS_LAT		: constant Long_Float := 53.71;
   DEFAULT_GS_LON		: constant Long_Float := -1.86;
   DEFAULT_GS_HT		: constant Long_Float := 500.0;

   PREF_GS_NAME			: constant String := "pref.gs_name";
   PREF_GS_LAT			: constant String := "pref.gs_lat";
   PREF_GS_LON			: constant String := "pref.gs_lon";
   PREF_GS_HT			: constant String := "pref.gs_ht";
   PREF_SAT_SELECT		: constant String := "pref.sel_sat";

   Prefs_Filename		: constant String := 
                                    EarthStation.Platform.Get_Preferences_Directory
                                    & "prefs";
   Prefs			: Config_File.Config_Data;
   Shutting_Down		: Boolean := False;

   --------------------------------
   -- Create_Default_Preferences --
   --------------------------------

   procedure Create_Default_Preferences (This : in out Config_File.Config_Data) is
   begin
      Config_File.Set_String (This, PREF_GS_NAME, DEFAULT_GS_NAME);
      Config_File.Set_Long_Float (This, PREF_GS_LAT, DEFAULT_GS_LAT);
      Config_File.Set_Long_Float (This, PREF_GS_LON, DEFAULT_GS_LON);
      Config_File.Set_Long_Float (This, PREF_GS_HT, DEFAULT_GS_HT);
      Config_File.Set_String (This, PREF_SAT_SELECT, "");

      declare
         Result	: Message_Dialog_Buttons;
         pragma Unreferenced (Result);
      begin
         Result := Gtkada.Dialogs.Message_Dialog
           (Msg			=> "Using default preferences",
            Dialog_Type		=> Warning,
            Buttons		=> Button_OK,
            Default_Button	=> Button_OK);

         Save_Preferences (This);      
      end;
   end Create_Default_Preferences;

   ---------------
   -- Exit_Main --
   ---------------

   procedure Exit_Main (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      pragma Unreferenced (Object);
      Shutting_Down := True;
      Main_Quit;
   end Exit_Main;

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      pragma Unreferenced (Object);
      Shutting_Down := True;
      Main_Quit;
   end Exit_Main;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (File		: access File_Type;
      Max_Length	: in Positive := 256) return String
   is
      Line_Length	: Natural;
      Temp_String	: String (1 .. Max_Length);
   begin
      Get_Line (File.all, Temp_String, Line_Length);
      return Trim_Nonprint (Temp_String (1 .. Line_Length));
   end Get_String;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (This : out Main_Window) is
   begin
      This := new Main_Window_Record;
      Initialize (This);
   end Gtk_New;

   -------------------------------------
   -- Handle_Clear_Keplerian_Elements --
   -------------------------------------

   procedure Handle_Clear_Keplerian_Elements
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window)
   is
      pragma Unreferenced (Object);
      Result		: Message_Dialog_Buttons;
   begin
      Result := Gtkada.Dialogs.Message_Dialog
        (Msg		=> "Are you sure you wish to clear Keplerian Elements?" & 
                    	   ASCII.LF &
                   	   "After doing this you will need to import a TLE File.",
         Dialog_Type	=> Confirmation,
         Buttons	=> Button_Yes or Button_No,
         Default_Button	=> Button_No);

      if Result = Button_Yes then
         Iterate_Satellite_Names (Handle_Clear_Keplerian_Iter'Access);
         EarthStation.Tracking.Clear (User_Data.all.Data);
         Update_Tracking_Menu (User_Data);
      end if;
   end Handle_Clear_Keplerian_Elements;

   ---------------------------------
   -- Handle_Clear_Keplerian_Iter --
   ---------------------------------

   function Handle_Clear_Keplerian_Iter
     (Satellite_Name	: in String) return Boolean
   is
      Filename		: constant String :=
        EarthStation.Platform.Get_Keplerian_Elements_Directory &
        To_Filename (Satellite_Name);
      Junk		: Boolean;
   begin
      Delete_File (Filename, Junk);
      pragma Unreferenced (Junk);
      return True;
   end Handle_Clear_Keplerian_Iter;

   --------------------------------------
   -- Handle_Groundstation_Menu_Select --
   --------------------------------------

   procedure Handle_Groundstation_Menu_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window)
   is
      pragma Unreferenced (Object);
      package ESGD renames EarthStation.Groundstation_Dialogue;
      GS_Dialogue	: ESGD.Groundstation_Dialogue;
      Result		: Message_Dialog_Buttons;
      pragma Unreferenced (Result);
   begin
      Gtk_New (GS_Dialogue);
      ESGD.Set_GS_Name
        (GS_Dialogue, Config_File.Get_String (Prefs, PREF_GS_NAME, False, DEFAULT_GS_NAME));
      ESGD.Set_Latitude
        (GS_Dialogue, Config_File.Get_Long_Float (Prefs, PREF_GS_LAT, False, DEFAULT_GS_LAT));
      ESGD.Set_Longitude
        (GS_Dialogue, Config_File.Get_Long_Float (Prefs, PREF_GS_LON, False, DEFAULT_GS_LON));
      ESGD.Set_Height
        (GS_Dialogue, Config_File.Get_Long_Float (Prefs, PREF_GS_HT, False, DEFAULT_GS_HT));

      loop
         begin
            if Run (GS_Dialogue) = Gtk_Response_OK then
               Config_File.Set_String
                 (Prefs, PREF_GS_NAME, ESGD.Get_GS_Name (GS_Dialogue));
               Config_File.Set_Long_Float
                 (Prefs, PREF_GS_LAT, ESGD.Get_Latitude (GS_Dialogue));
               Config_File.Set_Long_Float
                 (Prefs, PREF_GS_LON, ESGD.Get_Longitude (GS_Dialogue));
               Config_File.Set_Long_Float
                 (Prefs, PREF_GS_HT, ESGD.Get_Height (GS_Dialogue));
               exit;
            else
               exit;
            end if;
         exception
            when CONSTRAINT_ERROR =>
               Result := Gtkada.Dialogs.Message_Dialog
                 (Msg			=> "Invalid Value",
                  Dialog_Type		=> Error,
                  Buttons		=> Button_OK,
                  Default_Button	=> Button_OK);
         end;
      end loop;

      EarthStation.Tracking.Initialize
        (This			=> User_Data.Data,
         Groundstation_Name	=> Config_File.Get_String (Prefs, PREF_GS_NAME),
         Latitude		=> Config_File.Get_Long_Float (Prefs, PREF_GS_LAT),
         Longitude		=> Config_File.Get_Long_Float (Prefs, PREF_GS_LON),
         Height			=> Config_File.Get_Long_Float (Prefs, PREF_GS_HT));

      Save_Preferences (Prefs);
      Destroy (GS_Dialogue);
   end Handle_Groundstation_Menu_Select;

   -----------------------
   -- Handle_Import_TLE --
   -----------------------

   procedure Handle_Import_TLE
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window)
   is
      pragma Unreferenced (Object);
      pragma Unreferenced (User_Data);
   begin
      declare
         Filename	: constant String :=
                            File_Selection_Dialog
                              ("Import TLE File", "", False, True);
      begin
         if Filename /= "" then
            Handle_Import_TLE_OK (Filename);
         end if;
      end;
   end Handle_Import_TLE;

   --------------------------
   -- Handle_Import_TLE_OK --
   --------------------------

   procedure Handle_Import_TLE_OK (Filename : in String)
   is
      File	: aliased File_Type;
      L1	: Unbounded_String;
      L2	: Unbounded_String;
      L3	: Unbounded_String;
      Num_Imp	: Natural := 0;
      Result	: Message_Dialog_Buttons;
      pragma Unreferenced (Result);
   begin

      Open (File, In_File, Filename);

      --  Find the start of the TLE data; some sources add a header which
      --  needs to be ignored.

      Set_Unbounded_String (L1, Get_String (File'Access));
      Set_Unbounded_String (L2, Get_String (File'Access));
      Set_Unbounded_String (L3, Get_String (File'Access));

      if not Looks_Like_TLE (L1, L2, L3) then
         loop
            L1 := L2;
            L2 := L3;
            Set_Unbounded_String (L3, Get_String (File'Access));
            exit when Looks_Like_TLE (L1, L2, L3);
         end loop;
      end if;

      --  L1/L2/L3 should now contain a valid TLE set

      loop
         declare
            K		: EarthStation.Predict.Keplerian_Elements;
         begin
            K := TLE_To_Keplerian_Elements (To_String (L2), To_String (L3));
            Save (To_String (L1), K);
            Num_Imp := Num_Imp + 1;
         exception
            when others =>
               Result := Gtkada.Dialogs.Message_Dialog
                 (Msg			=> "Unable to import TLE for: " 
                                             & To_String (L1),
                  Dialog_Type		=> Warning,
                  Buttons		=> Button_OK,
                  Default_Button	=> Button_OK);
         end;

         exit when End_Of_File (File);

         Set_Unbounded_String (L1, Get_String (File'Access));
         exit when L1 = "/EX";	--  AMSAT keps have this at EOF
         Set_Unbounded_String (L2, Get_String (File'Access));
         Set_Unbounded_String (L3, Get_String (File'Access));
      end loop;

      Close (File);

      Result := Gtkada.Dialogs.Message_Dialog
        (Msg			=> "Imported"  & Natural'Image (Num_Imp)
                                     & " Elements",
         Dialog_Type		=> Information,
         Buttons		=> Button_OK,
         Default_Button		=> Button_OK);

   exception
      when Ada.IO_Exceptions.END_ERROR =>
         Result := Gtkada.Dialogs.Message_Dialog
           (Msg			=> "Unexpected EOF reading: " & Filename
                                     & ASCII.LF
                                     & " Imported" & Natural'Image (Num_Imp)
                                     & " Elements",
            Dialog_Type		=> Warning,
            Buttons		=> Button_OK,
            Default_Button	=> Button_OK);

            Close (File);
      when Ada.IO_Exceptions.NAME_ERROR =>
         Result := Gtkada.Dialogs.Message_Dialog
           (Msg			=> "Unable to Open File: " & Filename,
            Dialog_Type		=> Error,
            Buttons		=> Button_OK,
            Default_Button	=> Button_OK);
   end Handle_Import_TLE_OK;

   --------------------
   -- Handle_Timeout --
   --------------------

   function Handle_Timeout (This : in Main_Window) return Boolean is
   begin
      if not Shutting_Down then
         EarthStation.Map_Display.Update_Start (This.Map);
         EarthStation.Tracking.Update_Display
           (This.Data, This.Map, This.Satellite_Data);
         EarthStation.Map_Display.Update_End (This.Map);
      end if;

      return True;
   end Handle_Timeout;

   ------------------------------
   -- Handle_Track_Menu_Select --
   ------------------------------

   procedure Handle_Track_Menu_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     EarthStation.Tracking.Data_Access)
   is
      Result		: Message_Dialog_Buttons;
      pragma Unreferenced (Result);
   begin
      EarthStation.Tracking.Select_Satellite (User_Data.all, Object);
      Config_File.Set_String
        (Prefs, PREF_SAT_SELECT, Get_Selected_Satellite (User_Data.all));
     Save_Preferences (Prefs);
   end Handle_Track_Menu_Select;

   ----------------------------
   -- Handle_Tracking_Select --
   ----------------------------

   procedure Handle_Tracking_Select
     (Object		: access Gtk_Menu_Item_Record'Class;
      User_Data		: in     Main_Window)
   is
      pragma Unreferenced (Object);
      Track_Dialogue	: EarthStation.Select_Satellite.Select_Satellite;
   begin
      Gtk_New (Track_Dialogue, User_Data.Data'Access);
      if Run (Track_Dialogue) = Gtk_Response_OK then
         Update_Tracking_List (Track_Dialogue, User_Data.Data);
         Update_Tracking_Menu (User_Data);
         EarthStation.Tracking.Save (User_Data.Data);
      end if;
      Destroy (Track_Dialogue);
   end Handle_Tracking_Select;

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
      Gtk_New (This.Map, EarthStation.Platform.Get_Share_Directory & "map.jpg");

      Gtk_New (This.Satellite_Data);
      Gtk_New (This.Status_Bar);

      Set_Title (This, "EarthStation");
      Set_Default_Size (This, 800, 500);
      Set_Position (This, Win_Pos_Center);

      --  Setup main menu
      --  File Menu
      Gtk_New (This.File_Menu);
      Gtk_New_With_Mnemonic (Menu_Item, "Import _TLE File...");
      Menu_Item_Window_Callback.Connect
        (Menu_Item,
         "activate",
         Menu_Item_Window_Callback.Marshallers.Void_Marshaller.To_Marshaller
           (Handle_Import_TLE'Access), User_Data => Main_Window (This));
      Append (This.File_Menu, Menu_Item);

      Gtk_New_With_Mnemonic (Menu_Item, "Clear Keplerian Elements");
      Menu_Item_Window_Callback.Connect
        (Menu_Item,
         "activate",
         Menu_Item_Window_Callback.Marshallers.Void_Marshaller.To_Marshaller
           (Handle_Clear_Keplerian_Elements'Access), User_Data => Main_Window (This));
      Append (This.File_Menu, Menu_Item);

      Gtk_New_With_Mnemonic (Menu_Item, "E_xit");
      Menu_Item_Callback.Connect
        (Menu_Item, "activate", Menu_Item_Callback.To_Marshaller (Exit_Main'Access));
      Append (This.File_Menu, Menu_Item);

      Gtk_New_With_Mnemonic (Menu_Item, "_File");
      Append (This.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, This.File_Menu);

      --  Edit Menu
      Gtk_New (This.Edit_Menu);

      Gtk_New_With_Mnemonic (Menu_Item, "_Groundstation...");
      Menu_Item_Window_Callback.Connect
        (Menu_Item, 
         "activate", 
         Menu_Item_Window_Callback.Marshallers.Void_Marshaller.To_Marshaller
          (Handle_Groundstation_Menu_Select'Access), User_Data => Main_Window (This));
      Append (This.Edit_Menu, Menu_Item);

      Gtk_New_With_Mnemonic (Menu_Item, "_Tracking...");
      Menu_Item_Window_Callback.Connect
        (Menu_Item, 
         "activate", 
         Menu_Item_Window_Callback.Marshallers.Void_Marshaller.To_Marshaller
          (Handle_Tracking_Select'Access), User_Data => Main_Window (This));
      Append (This.Edit_Menu, Menu_Item);

      Gtk_New_With_Mnemonic (Menu_Item, "_Edit");
      Append (This.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, This.Edit_Menu);

      --  View Menu
      Gtk_New (This.View_Menu);
      Gtk_New_With_Mnemonic (This.Active_Track, "_Active Track");
      Append (This.View_Menu, This.Active_Track);
     
      Gtk_New_With_Mnemonic (Menu_Item, "_View");
      Append (This.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, This.View_Menu);

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


      Initialize_Directories;
      Load_Preferences (Prefs);
      EarthStation.Tracking.Load (This.Data);
      EarthStation.Tracking.Select_Satellite
        (This.Data, Config_File.Get_String (Prefs, PREF_SAT_SELECT, False, ""));

      EarthStation.Tracking.Initialize
        (This			=> This.Data,
         Groundstation_Name	=> Config_File.Get_String
                                     (Prefs, PREF_GS_NAME, False, DEFAULT_GS_NAME),
         Latitude		=> Config_File.Get_Long_Float
                                     (Prefs, PREF_GS_LAT, False, DEFAULT_GS_LAT),
         Longitude		=> Config_File.Get_Long_Float
                                     (Prefs, PREF_GS_LON, False, DEFAULT_GS_LON),
         Height			=> Config_File.Get_Long_Float
                                     (Prefs, PREF_GS_HT, False, DEFAULT_GS_HT));

      Show_All (This);
      Update_Tracking_Menu (This);

      Timeout := Main_Window_Timeout.Timeout_Add
        (350, Handle_Timeout'Access, Main_Window (This));
   end Initialize;

   ----------------------------
   -- Initialize_Directories --
   ----------------------------

   procedure Initialize_Directories is
   begin
      Create_Home_Directory;
      Create_Keplerian_Elements_Directory;
      Create_Preferences_Directory;
   end Initialize_Directories;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (Config : in out Config_File.Config_Data) is
   begin
      Config_File.Load (Config, Prefs_Filename);
   exception
      when others =>
         Create_Default_Preferences (Config);
   end Load_Preferences;

   --------------------
   -- Looks_Like_TLE --
   --------------------

   function Looks_Like_TLE
     (Name		: in Unbounded_String;
      Line_1		: in Unbounded_String;
      Line_2		: in Unbounded_String) return Boolean
   is
   begin
      if Length (Name) > 0 and then Length (Name) < 33 and then
        Length (Line_1) = 69 and then Length (Line_2) = 69 and then
        Element (Line_1, 1) = '1' and then Element (Line_2, 1) = '2' then 
         return True;
      else
         return False;
      end if;
   end Looks_Like_TLE;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences (Config : in out Config_File.Config_Data) is
      Result	: Message_Dialog_Buttons;
      pragma Unreferenced (Result);
   begin
      Config_File.Save (Config, Prefs_Filename);
   exception
      when others =>
         Result := Gtkada.Dialogs.Message_Dialog
           (Msg			=> "Unable to save preferences",
            Dialog_Type		=> Error,
            Buttons		=> Button_OK,
            Default_Button	=> Button_OK);
   end Save_Preferences;

   --------------------
   -- Show_About_Box --
   --------------------

   procedure Show_About_Box (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      pragma Unreferenced (Object);
      EarthStation.About_Box.Run;
   end Show_About_Box;

   --------------------------
   -- Update_Tracking_Menu --
   --------------------------

   procedure Update_Tracking_Menu (This : access Main_Window_Record'Class) is
   begin
      if This.Active_Track_Menu /= null then
         Destroy (This.Active_Track_Menu);
      end if;

      This.Active_Track_Menu := EarthStation.Tracking.Allocate_Track_Menu
        (This.Data'Access, Handle_Track_Menu_Select'Access);
      Set_Submenu (This.Active_Track, This.Active_Track_Menu);
      Show_All (This.Active_Track_Menu);
   end Update_Tracking_Menu;

end EarthStation.Main_Window;

