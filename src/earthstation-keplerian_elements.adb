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
--  earthstation-keplerian_elements.adb		jvinters	26-May-2010
--

pragma License (GPL);

with Ada.Characters.Handling;		use Ada.Characters.Handling;
with Ada.Directories;			use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings;			use Ada.Strings;
with Ada.Strings.Fixed;			use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with EarthStation.Platform;		use EarthStation.Platform;

package body EarthStation.Keplerian_Elements is

   Hex_Digits	: constant String := "0123456789abcdef";

   -------------------
   -- From_Filename --
   -------------------

   function From_Filename (Input : in String) return String is
      Char	: Character;
      Next	: Character;
      Next_2	: Character;
      Pos	: Natural := Input'First;
      Result	: Unbounded_String;
   begin
      loop
         exit when Pos > Input'Length;
         Char := Input (Pos);
         if Char /= '%' then
            Append (Result, Char);
            Pos := Pos + 1;
         else
            Pos := Pos + 1;
            if Pos > Input'Length then
               Append (Result, '%');
               exit;
            else
               Next := Input (Pos);
               if Next = '%' then
                  Append (Result, '%');
                  Pos := Pos + 1;
               else
                  Pos := Pos + 1;
                  if Pos > Input'Length then
                     Append (Result, '%');
                     Append (Result, Next);
                     exit;
                  else
                     Next_2 := Input (Pos);
                     Pos := Pos + 1;
                     Append (Result, Hex_Convert (Next, Next_2));
                  end if;
               end if;
            end if;
         end if;
      end loop;

      return To_String (Result);
   end From_Filename;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (File : access File_Type) return Integer
   is
      Temp_Str		: constant String := Get_String (File);
   begin
      return Integer'Value (Temp_Str);
   end Get_Integer;

   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float (File : access File_Type) return Long_Float
   is
      Temp_Str		: constant String := Get_String (File);
   begin
      return Long_Float'Value (Temp_Str);
   end Get_Long_Float;

   ----------------------
   -- Get_Long_Integer --
   ----------------------

   function Get_Long_Integer (File : access File_Type) return Long_Integer
   is
      Temp_Str		: constant String := Get_String (File);
   begin
      return Long_Integer'Value (Temp_Str);
   end Get_Long_Integer;

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
      return Trim (Temp_String (1 .. Line_Length), Both);
   end Get_String;

   -----------------
   -- Hex_Convert --
   -----------------

   function Hex_Convert
      (C1		: in Character;
       C2		: in Character) return String
   is

      function Hex_Value (Char : in Character) return Natural is
      begin
         for i in Hex_Digits'Range loop
            if Hex_Digits (i) = Char then
               return i - Hex_Digits'First;
            end if;
         end loop;
         raise CONSTRAINT_ERROR;
      end Hex_Value;

      CV		: Natural;
      C1V		: Natural;
      C2V		: Natural;

   begin
      C1V := Hex_Value (C1);
      C2V := Hex_Value (C2);
      CV := (C1V * 16) + C2V;
      return "" & Character'Val (CV);
   exception
      when CONSTRAINT_ERROR =>
         return '%' & C1 & C2;
   end Hex_Convert;

   -----------------------------
   -- Iterate_Satellite_Names --
   -----------------------------

   procedure Iterate_Satellite_Names (Iter : in Kep_Iterator) is
      De	: Directory_Entry_Type;
      St	: Search_Type;
   begin
      Start_Search
        (Search		=> St,
         Directory	=> EarthStation.Platform.Get_Keplerian_Elements_Directory,
         Pattern	=> "*");

      loop
         exit when not More_Entries (St);
         Get_Next_Entry
           (Search		=> St,
            Directory_Entry	=> De);

         declare
            Fn	: constant String := From_Filename (Simple_Name (De));
         begin
            if Fn /= "." and then Fn /= ".." then
               exit when not Iter.all (Fn);
            end if;
         end;
      end loop;

      End_Search (St);
   exception
      when Ada.IO_Exceptions.NAME_ERROR =>
         End_Search (St);
   end Iterate_Satellite_Names;

   ----------
   -- Load --
   ----------

   function Load (Name : in String) 
     return EarthStation.Predict.Keplerian_Elements
   is
      Closed	: Boolean := True;
      File	: aliased File_Type;
      Filename	: constant String :=
        EarthStation.Platform.Get_Keplerian_Elements_Directory &
        To_Filename (Name);
      Temp	: EarthStation.Predict.Keplerian_Elements;
   begin
      Open (File, In_File, Filename);
      Closed := False;

      declare
         Name		: constant String := Get_String (File'Access);
         pragma Unreferenced (Name);
      begin
         Temp.Epoch_Year := Get_Integer (File'Access);
         Temp.Epoch_Time := Get_Long_Float (File'Access);
         Temp.Inclination := Get_Long_Float (File'Access);
         Temp.RAAN := Get_Long_Float (File'Access);
         Temp.Eccentricity := Get_Long_Float (File'Access);
         Temp.Argument_Perigee := Get_Long_Float (File'Access);
         Temp.Mean_Anomaly := Get_Long_Float (File'Access);
         Temp.Mean_Motion := Get_Long_Float (File'Access);
         Temp.Decay_Rate := Get_Long_Float (File'Access);
         Temp.Orbit_Number := Get_Long_Integer (File'Access);
         Temp.ALON := Get_Long_Float (File'Access);
         Temp.ALAT := Get_Long_Float (File'Access);

         Close (File);
         return Temp;
      end;
   exception
      when Ada.IO_Exceptions.NAME_ERROR =>
         raise SATELLITE_NOT_FOUND;
      when others =>
         if not Closed then
            Close (File);
         end if;
         raise;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Name		: in String;
      Elements		: in EarthStation.Predict.Keplerian_Elements)
   is
      Closed		: Boolean := True;
      File		: File_Type;
      Filename		: constant String :=
        EarthStation.Platform.Get_Keplerian_Elements_Directory &
        To_Filename (Name);
   begin
      Create (File, Out_File, Filename);
      Closed := False;

      Put_Line (File, Name);
      Put_Line (File, Integer'Image (Elements.Epoch_Year));
      Put_Line (File, Long_Float'Image (Elements.Epoch_Time));
      Put_Line (File, Long_Float'Image (Elements.Inclination));
      Put_Line (File, Long_Float'Image (Elements.RAAN));
      Put_Line (File, Long_Float'Image (Elements.Eccentricity));
      Put_Line (File, Long_Float'Image (Elements.Argument_Perigee));
      Put_Line (File, Long_Float'Image (Elements.Mean_Anomaly));
      Put_Line (File, Long_Float'Image (Elements.Mean_Motion));
      Put_Line (File, Long_Float'Image (Elements.Decay_Rate));
      Put_Line (File, Long_Integer'Image (Elements.Orbit_Number));
      Put_Line (File, Long_Float'Image (Elements.ALON));
      Put_Line (File, Long_Float'Image (Elements.ALAT));

      Close (File);
   exception
      when others =>
         if not Closed then
            Close (File);
         end if;
         raise;
   end Save;

   ------------------------
   -- TLE_Checksum_Valid --
   ------------------------

   function TLE_Checksum_Valid (This : in String) return Boolean is
      Char	: Character;
      Sum	: Natural := 0;
      Zero_Pos	: constant Natural := Character'Pos ('0');
   begin
      if This'Length /= 69 then
         return False;
      else
         for i in This'First .. This'Last - 1 loop
            Char := This (i);
            if Char in '0'..'9' then
               Sum := Sum + (Character'Pos (Char) - Zero_Pos);
            elsif Char = '-' then
               Sum := Sum + 1;
            end if;
         end loop;

         Sum := Sum mod 10;
         Char := Character'Val (Sum + Zero_Pos);
         return Char = This (This'Last);
      end if;
   end TLE_Checksum_Valid;

   -------------------------------
   -- TLE_To_Keplerian_Elements --
   -------------------------------

   function TLE_To_Keplerian_Elements
     (Line_1	: in String;
      Line_2	: in String) return EarthStation.Predict.Keplerian_Elements
   is
      Temp	: EarthStation.Predict.Keplerian_Elements;
   begin
      if Line_1'Length /= 69 or else Line_2'Length /= 69 then
         raise CONSTRAINT_ERROR;
      end if;

      declare
         Epoch_Year_Str	: constant String := "20" & 
                            Line_1 (Line_1'First + 18 .. Line_1'First + 19);
         Epoch_Time_Str	: constant String := 
                            Line_1 (Line_1'First + 20 .. Line_1'First + 31);
         Inclin_Str	: constant String :=
                            Line_2 (Line_2'First + 8 .. Line_2'First + 15);
         RAAN_Str	: constant String :=
                            Line_2 (Line_2'First + 17 .. Line_2'First + 24);
         Eccen_Str	: constant String := "0." &
                            Line_2 (Line_2'First + 26 .. Line_2'First + 32);
         AP_Str		: constant String := 
                            Line_2 (Line_2'First + 34 .. Line_2'First + 41);
         MA_Str		: constant String :=
                            Line_2 (Line_2'First + 43 .. Line_2'First + 50);
         MM_Str		: constant String :=
                            Line_2 (Line_2'First + 52 .. Line_2'First + 62);
         Orbit_Str	: constant String :=
                            Line_2 (Line_2'First + 63 .. Line_2'First + 67);
      begin
         Temp.Epoch_Year := Integer'Value (Epoch_Year_Str);
         Temp.Epoch_Time := Long_Float'Value (Epoch_Time_Str);
         Temp.Inclination := Long_Float'Value (Inclin_Str);
         Temp.RAAN := Long_Float'Value (RAAN_Str);
         Temp.Eccentricity := Long_Float'Value (Eccen_Str);
         Temp.Argument_Perigee := Long_Float'Value (AP_Str);
         Temp.Mean_Anomaly := Long_Float'Value (MA_Str);
         Temp.Mean_Motion := Long_Float'Value (MM_Str);
         Temp.Orbit_Number := Long_Integer'Value (Orbit_Str);
         Temp.ALON := 0.0;
         Temp.ALAT := 0.0;
         return Temp;
      end;
   end TLE_To_Keplerian_Elements;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (Input : in String) return String is
      Temp	: Unbounded_String;
      Trim_Inp	: constant String := To_Upper (Trim_Nonprint (Input));
   begin
      if Input'Length = 0 then
         raise CONSTRAINT_ERROR;
      end if;

      for i in Trim_Inp'Range loop
         case Trim_Inp (i) is
            when 'a'..'z' =>
               Append (Temp, Trim_Inp (i));
            when 'A'..'Z' =>
               Append (Temp, Trim_Inp (i));
            when '0'..'9' =>
               Append (Temp, Trim_Inp (i));
            when '%' =>
               Append (Temp, "%%");
            when others =>
               Append (Temp, To_Hex (Trim_Inp (i)));
         end case;
      end loop;

      return To_String (Temp);
   end To_Filename;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (This : in Character) return String is
      CPos	: constant Integer := Character'Pos (This);
      C1	: constant Integer := CPos mod 16;
      C2	: constant Integer := CPos / 16;
   begin
      return '%' &
        Hex_Digits (Hex_Digits'First + C2) &
        Hex_Digits (Hex_Digits'First + C1);
   end To_Hex;

   -------------------
   -- Trim_Nonprint --
   -------------------

   function Trim_Nonprint (This : in String) return String is
      Left	: Natural := This'Last;
      Right	: Natural := This'First;
   begin
      --  First, find the first printable character on the left

      for i in This'Range loop
         if Character'Pos (This (i)) > 32 and then
           Character'Pos (This (i)) <= 126 then
            Left := i;
            exit;
         end if;
      end loop;

      if Left = This'Last then
         return "";
      end if;

      for i in reverse This'Range loop
         if Character'Pos (This (i)) > 32 and then
           Character'Pos (This (i)) <= 126 then
            Right := i;
            exit;
         end if;
      end loop;

      return This (Left .. Right);
   end Trim_Nonprint;

end EarthStation.Keplerian_Elements;

