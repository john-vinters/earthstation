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
-- As a special exception under Section 7 of GPL version 3, you are granted
-- additional permissions described in the GCC Runtime Library Exception,
-- version 3.1, as published by the Free Software Foundation.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, see <http://www.gnu.org/licenses/>.
--
--  generic_3d_vectors.adb	jvinters	2-May-2010
--

pragma License (Modified_GPL);

with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_3D_Vectors is

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);

   ---------
   -- "+" --
   ---------

   function "+"
     (Left	: in Vector;
      Right	: in Vector) return Vector
   is
      Temp	: Vector;
   begin
      Temp.X := Left.X + Right.X;
      Temp.Y := Left.Y + Right.Y;
      Temp.Z := Left.Z + Right.Z;
      return Temp;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  	: in Vector;
      Right 	: in Vector) return Vector
   is
      Temp	: Vector;
   begin
      Temp.X := Left.X - Right.X;
      Temp.Y := Left.Y - Right.Y;
      Temp.Z := Left.Z - Right.Z;
      return Temp;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left	: in Vector;
      Right	: in Real'Base) return Vector
   is
      Temp	: Vector;
   begin
      Temp.X := Left.X * Right;
      Temp.Y := Left.Y * Right;
      Temp.Z := Left.Z * Right;
      return Temp;
   end "*";

   -----------------
   -- Dot_Product --
   -----------------

   function Dot_Product
     (Left	: in Vector;
      Right	: in Vector) return Real'Base
   is
   begin
      return (Left.X * Right.X) + (Left.Y * Right.Y) + (Left.Z * Right.Z);
   end Dot_Product;

   ---------------
   -- Magnitude --
   ---------------

   function Magnitude (This : in Vector) return Real'Base is
   begin
      return Math.Sqrt ((This.X * This.X) + (This.Y * This.Y) + (This.Z * This.Z));
   end Magnitude;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (This : in Vector) return Vector is
      Mag 	: constant Real'Base := Magnitude (This);
      Temp	: Vector;
   begin
      Temp.X := This.X / Mag;
      Temp.Y := This.Y / Mag;
      Temp.Z := This.Z / Mag;
      return Temp;
   end Normalize;

end Generic_3D_Vectors;

