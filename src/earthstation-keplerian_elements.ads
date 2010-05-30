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
--  earthstation-keplerian_elements.ads		jvinters	26-May-2010
--

pragma License (GPL);

with Ada.Text_IO;			use Ada.Text_IO;
with EarthStation.Predict;

package EarthStation.Keplerian_Elements is

   SATELLITE_NOT_FOUND		: Exception;

   type Kep_Iterator is access function
     (Satellite_Name		: in String) return Boolean;
   --  Satellite name iterator.  If the function returns False, then the
   --  iteration will stop.

   function From_Filename (Input : in String) return String;
   --  Converts filename to satellite name

   procedure Iterate_Satellite_Names (Iter : in Kep_Iterator);
   --  Iterates through elements, passing each satellite name to iterator.

   function Load (Name : in String) return EarthStation.Predict.Keplerian_Elements;
   --  Loads keplerian elements for given satellite name.
   --  May raise SATELLITE_NOT_FOUND if the given name can't be found.

   procedure Save
     (Name		: in String;
      Elements		: in EarthStation.Predict.Keplerian_Elements);
   --  Saves keplerian elements for given satellite name.
   --  May raise exceptions if elements can't be saved, or CONSTRAINT_ERROR
   --  if the name is an empty string.

   function TLE_Checksum_Valid (This : in String) return Boolean;
   --  Checks that the TLE Checksum is valid.  Returns True if so, false 
   --  otherwise.

   function TLE_To_Keplerian_Elements
     (Line_1	: in String;
      Line_2	: in String) return EarthStation.Predict.Keplerian_Elements;
   --  Converts 2 Line Elements to Keplerian Elements.
   --  May raise CONSTRAINT_ERROR if there is a problem with the TLEs.

   function To_Filename (Input : in String) return String;
   --  Converts satellite name to filename

private

   function Get_Integer (File : access File_Type) return Integer;
   --  Reads an Integer from file.  May raise CONSTRAINT_ERROR if the
   --  line read isn't a valid Integer value.  Other Exceptions may be
   --  raised if we can't read the file.

   function Get_Long_Float (File : access File_Type) return Long_Float;
   --  Reads a Long_Float from file.  May raise CONSTRAINT_ERROR if the
   --  line read isn't a valid Long_Float value.  Other Exceptions may be
   --  raised if we can't read the file.

   function Get_Long_Integer (File : access File_Type) return Long_Integer;
   --  Reads a Long_Integer from file.  May raise CONSTRAINT_ERROR if the
   --  line read isn't a valid Long_Integer value.  Other Exceptions may be
   --  raised if we can't read the file.

   function Get_String
     (File		: access File_Type;
      Max_Length	: in Positive := 256) return String;
   --  Reads a String from file.  May raise Exceptions if we can't read file.
   --  Whitespace is trimmed from the beginning and end of the returned string.

   function Hex_Convert
     (C1		: in Character;
      C2		: in Character) return String;
   --  Converts Hex Digits into a single character string (if valid),
   --  otherwise to string '%' & 'C1' & 'C2'.

   function To_Hex (This : in Character) return String;
   --  Converts character to '%hh' form where h represents a hex digit

end EarthStation.Keplerian_Elements;

