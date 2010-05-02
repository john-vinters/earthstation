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
--  generic_3d_vectors.ads	jvinters	2-May-2010
--

pragma License (Modified_GPL);

generic

   type Real is digits <>;

package Generic_3D_Vectors is

   type Vector is record
      X		: Real'Base;
      Y		: Real'Base;
      Z		: Real'Base;
   end record;

   function "+"
     (Left	: in Vector;
      Right	: in Vector) return Vector;
   --  Addition Operator

   function "-"
     (Left  	: in Vector;
      Right 	: in Vector) return Vector;
   --  Subtraction Operator

   function "*"
     (Left	: in Vector;
      Right	: in Real'Base) return Vector;
   --  Scalar Multiply

   function Dot_Product
     (Left	: in Vector;
      Right	: in Vector) return Real'Base;
   --  Dot Product

   function Magnitude (This : in Vector) return Real'Base;
   --  Returns vector magnitude

   function Normalize (This : in Vector) return Vector;
   --  Returns normalized vector

end Generic_3D_Vectors;

