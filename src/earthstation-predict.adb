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
--  earthstation-predict.adb	jvinters	2-May-2010
--

pragma License (GPL);

with Ada.Calendar;			use Ada.Calendar;
with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;		use Ada.Calendar.Time_Zones;
with Ada.Numerics;

package body EarthStation.Predict is

   One_Third	: constant Long_Float := 1.0 / 3.0;

   ---------
   -- Atn --
   ---------

   function Atn
     (Y		: Long_Float;
      X		: Long_Float) return Long_Float
   is
      A		: Long_Float;
   begin
      if X /= 0.0 then
         A := Arctan (Y/X);
      else
         A := Pi / 2.0 * Sgn (Y);
      end if;

      if X < 0.0 then
         A := A + Pi;
      end if;

      if A < 0.0 then
         A := A + 2.0 * Pi;
      end if;

      return A;
   end Atn;

   -----------------------------
   -- Calculate_Range_Vectors --
   -----------------------------

   function Calculate_Range_Vectors
     (Sat		: in     Satellite;
      Groundstation	: in     Observer;
      Sat_Vectors	: in     Satellite_Vectors) return Range_Vectors
   is
      R			: Range_Vectors;
   begin
      --  Calculate Range Vector and Range Magnitude
      R.Range_Vector := Sat_Vectors.STG - Groundstation.Observer_Surface;
      R.Range_Magnitude := Magnitude (R.Range_Vector);
      R.Range_Vector := Normalize (R.Range_Vector);

      R.U := R.Range_Vector.X * Groundstation.Unit_Up.X +
        R.Range_Vector.Y * Groundstation.Unit_Up.Y +
          R.Range_Vector.Z * Groundstation.Unit_Up.Z;
      R.E := R.Range_Vector.X * Groundstation.Unit_East.X +
        R.Range_Vector.Y * Groundstation.Unit_East.Y;
      R.N := R.Range_Vector.X * Groundstation.Unit_North.X +
        R.Range_Vector.Y * Groundstation.Unit_North.Y +
          R.Range_Vector.Z * Groundstation.Unit_North.Z;

      --  Calculate Satellite Azimuth and Elevation
      R.Azimuth := Degrees (Atn (R.E, R.N));
      R.Elevation := Degrees (Arcsin (R.U));

      --  Calculate Subsatellite Latitude and Longitude
      R.Subsat_Longitude := Degrees (Atn (Sat_Vectors.STG.Y, Sat_Vectors.STG.X));
      R.Subsat_Latitude := Degrees (Arcsin (Sat_Vectors.STG.Z / Sat_Vectors.RS));

      R.Range_Rate := (Sat_Vectors.VLG.X - Groundstation.Observer_Velocity.X) * R.Range_Vector.X +
        (Sat_Vectors.VLG.Y - Groundstation.Observer_Velocity.Y) * R.Range_Vector.Y +
        Sat_Vectors.VLG.Z * R.Range_Vector.Z;

      R.Doppler_Factor := R.Range_Rate / 299792.0;

      --  If Sun is more than 10 degrees below Horizon, then Satellite is
      --  possibly visible

      if ((Sat_Vectors.H.X * Groundstation.Unit_Up.X) +
          (Sat_Vectors.H.Y * Groundstation.Unit_Up.Y) +
          (Sat_Vectors.H.Z * Groundstation.Unit_Up.Z)) < 0.17 and then
        Sat_Vectors.Visibility /= Eclipsed then
         R.Visible := True;
      else
         R.Visible := False;
      end if;

      return R;
   end Calculate_Range_Vectors;

   ---------------------------------
   -- Calculate_Satellite_Vectors --
   ---------------------------------

   function Calculate_Satellite_Vectors
     (This		: in     Satellite;
      Day_Number	: in     Long_Integer;
      Day_Fraction	: in     Long_Float) return Satellite_Vectors
   is
      C, S		: Long_Float;
      D, DNom		: Long_Float;
      EA		: Long_Float;
      V			: Satellite_Vectors;
   begin
      V.Time_Since_Epoch :=
        Long_Float (Day_Number - This.Epoch_Day) + (Day_Fraction - This.Epoch_Time);
      V.DN := Day_Number;

      --  Calculate Linear Drag Terms
      V.DT := This.Drag_Coefficient * V.Time_Since_Epoch / 2.0;
      V.KD := 1.0 + 4.0 * V.DT;
      V.KDP := 1.0 - 7.0 * V.DT;

      V.M :=
        This.Mean_Anomaly + This.Mean_Motion * V.Time_Since_Epoch *
          (1.0 - 3.0 * V.DT);
      V.DR := Long_Integer (Long_Float'Floor (V.M / (2.0 * Pi)));
      V.M := V.M - Long_Float (V.DR) * 2.0 * Pi;

      --  Calculate Orbit Number
      V.RN := This.Orbit_Number + V.DR;

      --  Solve M = EA - Eccentricity * Sin (EA) for EA given M using
      --  Newton's Method

      EA := V.M;				--  Initial Guess
      loop
         C := Cos (EA);
         S := Sin (EA);
         DNom := 1.0 - This.Eccentricity * C;
         D := (EA - This.Eccentricity * S - V.M) / DNom;
         EA := EA - D;
         exit when Abs (D) < 1.0E-5;		--  exit when close enough converged
      end loop;

      V.A := This.Semi_Major_Axis * V.KD;
      V.B := This.Semi_Minor_Axis * V.KD;
      V.RS := V.A * DNom;

      --  Calculate Satellite Position and Velocity within plane of oribtal ellipse
      V.Sx := V.A * (C - This.Eccentricity);
      V.Sy := V.B * S;
      V.Vx := -V.A * S / DNom * This.N0;
      V.Vy := V.B * C / DNom * This.N0;

      V.AP := This.Argument_Perigee + This.Perigee_Precession * V.Time_Since_Epoch * V.KDP;
      V.CW := Cos (V.AP);
      V.SW := Sin (V.AP);
      V.RAAN := This.RAAN + This.Node_Precession * V.Time_Since_Epoch * V.KDP;
      V.CQ := Cos (V.RAAN);
      V.SQ := Sin (V.RAAN);

      --  Convert Orbital Plane -> Celestial Coordinates
      V.CX.X := V.CW * V.CQ - V.SW * This.CI * V.SQ;
      V.CX.Y := -V.SW * V.CQ - V.CW * This.CI * V.SQ;
      V.CX.Z := This.SI * V.SQ;

      V.CY.X := V.CW * V.SQ + V.SW * This.CI * V.CQ;
      V.CY.Y := -V.SW * V.SQ + V.CW * This.CI * V.CQ;
      V.CY.Z := -This.SI * V.CQ;

      V.CZ.X := V.SW * This.SI;
      V.CZ.Y := V.CW * This.SI;
      V.CZ.Z := This.CI;

      --  Calculate Satellite Position Vector (CELESTIAL)
      V.SAT.X := V.Sx * V.CX.X + V.Sy * V.CX.Y;
      V.SAT.Y := V.Sx * V.CY.X + V.Sy * V.CY.Y;
      V.SAT.Z := V.Sx * V.CZ.X + V.Sy * V.CZ.Y;

      --  Calculate Satellite Velocity Vector (CELESTIAL)
      V.VEL.X := V.Vx * V.CX.X + V.Vy * V.CX.Y;
      V.VEL.Y := V.Vx * V.CY.X + V.Vy * V.CY.Y;
      V.VEL.Z := V.Vx * V.CZ.X + V.Vy * V.CZ.Y;

      --  Calculate Antenna Axis Unit Vector (CELESTIAL)
      V.ANT.X := This.Unit_Antenna.X * V.CX.X + This.Unit_Antenna.Y * V.CX.Y + This.Unit_Antenna.Z * V.CX.Z;
      V.ANT.Y := This.Unit_Antenna.X * V.CY.X + This.Unit_Antenna.Y * V.CY.Y + This.Unit_Antenna.Z * V.CY.Z;
      V.ANT.Z := This.Unit_Antenna.X * V.CZ.X + This.Unit_Antenna.Y * V.CZ.Y + This.Unit_Antenna.Z * V.CZ.Z;

      V.GHAA := This.GHAE + WE * V.Time_Since_Epoch;
      C := Cos (-V.GHAA);
      S := Sin (-V.GHAA);

      --  Convert Celestial SAT -> Geocentric
      V.STG.X := V.SAT.X * C - V.SAT.Y * S;
      V.STG.Y := V.SAT.X * S + V.SAT.Y * C;
      V.STG.Z := V.SAT.Z;

      --  Convert Celestial VEL -> Geocentric
      V.VLG.X := V.VEL.X * C - V.VEL.Y * S;
      V.VLG.Y := V.VEL.X * S + V.VEL.Y * C;
      V.VLG.Z := V.VEL.Z;

      --  Convert Celestial ANT -> Geocentric
      V.ALG.X := V.ANT.X * C - V.ANT.Y * S;
      V.ALG.Y := V.ANT.X * S + V.ANT.Y * C;
      V.ALG.Z := V.ANT.Z;

      --  Sun Vectors
      V.MAS := This.MASE + Radians (MASD * V.Time_Since_Epoch);
      V.TAS := This.MRSE + WW * V.Time_Since_Epoch +
        EQC1 * Sin (V.MAS) + EQC2 * Sin (2.0 * V.MAS);
      C := Cos (V.TAS);
      S := Sin (V.TAS);
      V.SUN.X := C;
      V.SUN.Y := S * CNS;
      V.SUN.Z := S * SNS;

      --  Find Solar Angle, Illumination and Eclipse Status
      V.SSA := -(V.ANT.X * V.SUN.X + V.ANT.Y * V.SUN.Y + V.ANT.Z * V.SUN.Z);
      V.Illumination := Sqrt (1.0 - V.SSA * V.SSA) * 100.0;
      V.CUA := -(V.SAT.X * V.SUN.X + V.SAT.Y * V.SUN.Y + V.SAT.Z * V.SUN.Z) / V.RS;
      if V.CUA >= 0.0 then
         V.Visibility := SHADE_SIDE;
      else
         V.Visibility := SUN_SIDE;
      end if;

      V.UMD := V.RS * Sqrt (1.0 - V.CUA * V.CUA) / RE;
      if V.UMD <= 1.0 and then V.CUA >= 0.0 then
         V.Visibility := ECLIPSED;
      end if;

      C := Cos(-V.GHAA);
      S := Sin(-V.GHAA);

      V.H.X := V.SUN.X * C - V.SUN.Y * S;
      V.H.Y := V.SUN.X * S + V.SUN.Y * C;
      V.H.Z := V.SUN.Z;

      --  Save the rest of visibility calculation for Calculate_Range_Vectors
      --  as we don't have the Observer's vectors available to use now...

      V.SUO.X := V.SUN.X * V.CX.X + V.SUN.Y * V.CY.X + V.SUN.Z * V.CZ.X;
      V.SUO.Y := V.SUN.X * V.CX.Y + V.SUN.Y * V.CY.Y + V.SUN.Z * V.CZ.Y;
      V.SUO.Z := V.SUN.X * V.CX.Z + V.SUN.Y * V.CY.Z + V.SUN.Z * V.CZ.Z;
      V.Sun_Elevation := Arcsin (V.SUO.Z);
      V.Sun_Azimuth := Atn (V.SUO.Y, V.SUO.X);

      return V;
   end Calculate_Satellite_Vectors;

   ------------------
   -- Clock_To_Day --
   ------------------

   procedure Clock_To_Day
     (Timestamp		: in     Ada.Calendar.Time;
      Day_Number	:    out Long_Integer;
      Day_Fraction	:    out Long_Float)
   is
      Adj_Timestamp	: Ada.Calendar.Time :=
        Timestamp - Duration (UTC_Time_Offset * 60);
      Year		: Ada.Calendar.Year_Number;
      Month		: Ada.Calendar.Month_Number;
      Day		: Ada.Calendar.Day_Number;
      Hours		: Ada.Calendar.Formatting.Hour_Number;
      Minutes		: Ada.Calendar.Formatting.Minute_Number;
      Seconds		: Ada.Calendar.Formatting.Second_Number;
   begin
      Year := Ada.Calendar.Formatting.Year (Timestamp);
      Month := Ada.Calendar.Formatting.Month (Timestamp);
      Day := Ada.Calendar.Formatting.Day (Adj_Timestamp);
      Day_Number := Date_To_Day (Year, Month, Day);

      Hours := Ada.Calendar.Formatting.Hour (Timestamp);
      Minutes := Ada.Calendar.Formatting.Minute (Timestamp);
      Seconds := Ada.Calendar.Formatting.Second (Timestamp);
      Day_Fraction := (
        (Long_Float (Hours) * 3600.0) +
        (Long_Float (Minutes) * 60.0) +
        (Long_Float (Seconds))) / 86400.0;
   end Clock_To_Day;

   -----------------
   -- Date_To_Day --
   -----------------

   function Date_To_Day
     (Year		: in Integer;
      Month		: in Integer;
      Day		: in Integer) return Long_Integer
   is
      Adjusted_Month	: Long_Float := Long_Float (Month);
      Adjusted_Year	: Long_Float := Long_Float (Year);
      Temp		: Long_Float;
   begin
      if Month <= 2 then
         Adjusted_Month := Adjusted_Month + 12.0;
         Adjusted_Year := Adjusted_Year - 1.0;
      end if;

      Temp :=
        Long_Float'Floor (Adjusted_Year * 365.25) +
        Long_Float'Floor ((Adjusted_Month + 1.0) * 30.6) +
        Long_Float (Day) + Long_Float (DATE_EPOCH);
      return Long_Integer (Temp);
   end Date_To_Day;

   -------------
   -- Degrees --
   -------------

   function  Degrees (Radians : in Long_Float) return Long_Float is
   begin
      return ((Radians * 180.0) / Ada.Numerics.Pi);
   end Degrees;

   -------------------
   -- Get_AMSAT_Day --
   -------------------

   function Get_AMSAT_Day (This : in Satellite_Vectors) return Long_Integer is
   begin
      return This.DN - 722100;
   end Get_AMSAT_Day;

   -----------------
   -- Get_Azimuth --
   -----------------

   function Get_Azimuth (This : in Range_Vectors) return Long_Float is
   begin
      return This.Azimuth;
   end Get_Azimuth;

   ------------------------
   -- Get_Doppler_Factor --
   ------------------------

   function Get_Doppler_Factor (This : in Range_Vectors) return Long_Float is
   begin
      return This.Doppler_Factor;
   end Get_Doppler_Factor;

   ------------------------
   -- Get_Eclipse_Status --
   ------------------------

   function Get_Eclipse_Status (This : in Satellite_Vectors) return Eclipse_Status is
   begin
      return This.Visibility;
   end Get_Eclipse_Status;

   -------------------
   -- Get_Elevation --
   -------------------

   function Get_Elevation (This : in Range_Vectors) return Long_Float is
   begin
      return This.Elevation;
   end Get_Elevation;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.RS - RE;
   end Get_Height;

   ----------------------
   -- Get_Illumination --
   ----------------------

   function Get_Illumination (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.Illumination;
   end Get_Illumination;

   ------------------
   -- Get_Latitude --
   ------------------

   function Get_Latitude (This : in Range_Vectors) return Long_Float is
   begin
      return This.Subsat_Latitude;
   end Get_Latitude;

   -------------------
   -- Get_Longitude --
   -------------------

   function Get_Longitude (This : in Range_Vectors) return Long_Float is
   begin
      return This.Subsat_Longitude;
   end Get_Longitude;

   ---------------
   -- Get_Orbit --
   ---------------

   function Get_Orbit (This : in Satellite_Vectors) return Long_Integer is
   begin
      return This.RN;
   end Get_Orbit;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range (This : in Range_Vectors) return Long_Float is
   begin
      return This.Range_Magnitude;
   end Get_Range;

   --------------------
   -- Get_Range_Rate --
   --------------------

   function Get_Range_Rate (This : in Range_Vectors) return Long_Float is
   begin
      return This.Range_Rate;
   end Get_Range_Rate;

   ------------
   -- Get_RS --
   ------------

   function Get_RS (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.RS;
   end Get_RS;

   ---------------------
   -- Get_Sun_Azimuth --
   ---------------------

   function Get_Sun_Azimuth (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.Sun_Azimuth;
   end Get_Sun_Azimuth;

   -----------------------
   -- Get_Sun_Elevation --
   -----------------------

   function Get_Sun_Elevation (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.Sun_Elevation;
   end Get_Sun_Elevation;

   --------------------------
   -- Get_Time_Since_Epoch --
   --------------------------

   function Get_Time_Since_Epoch (This : in Satellite_Vectors) return Long_Float is
   begin
      return This.Time_Since_Epoch;
   end Get_Time_Since_Epoch;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible (This : in Range_Vectors) return Boolean is
   begin
      return This.Visible;
   end Get_Visible;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility (This : in Satellite_Vectors) return Eclipse_Status is
   begin
      return This.Visibility;
   end Get_Visibility;

   -------------------------
   -- Initialize_Observer --
   -------------------------

   procedure Initialize_Observer
     (This	: in out Observer;
      Latitude	: in     Long_Float;
      Longitude	: in     Long_Float;
      Height	: in     Long_Float)
   is
   begin
      --  Convert Station Location Information into the correct units
      This.Latitude := Radians (Latitude);
      This.Longitude := Radians (Longitude);
      This.Height := Height / 1000.0;

      --  Precalculate Sin/Cos of Station Position
      This.CL := Cos (This.Latitude);
      This.SL := Sin (This.Latitude);
      This.CO := Cos (This.Longitude);
      This.SO := Sin (This.Longitude);

      --  Calculate Observer Unit East/North/Up Vectors (GEOCENTRIC)
      This.Unit_East.X := -This.SO;
      This.Unit_East.Y := This.CO;
      This.Unit_East.Z := 0.0;

      This.Unit_North.X := -This.SL * This.CO;
      This.Unit_North.Y := -This.SL * This.SO;
      This.Unit_North.Z := This.CL;

      This.Unit_Up.X := This.CL * This.CO;
      This.Unit_Up.Y := This.CL * This.SO;
      This.Unit_Up.Z := This.SL;

      --  Calculate Observer's XYZ Surface Coordinates
      This.D := Sqrt (XX * This.CL * This.CL + ZZ * This.SL * This.SL);
      This.Rx := XX / This.D + This.Height;
      This.Rz := ZZ / This.D + This.Height;

      This.Observer_Surface.X := This.Rx * This.Unit_Up.X;
      This.Observer_Surface.Y := This.Rx * This.Unit_Up.Y;
      This.Observer_Surface.Z := This.Rz * This.Unit_Up.Z;

      --  Calculate Observer's Velocity (GEOCENTRIC)
      This.Observer_Velocity.X := -This.Observer_Surface.Y * W0;
      This.Observer_Velocity.Y := This.Observer_Surface.X * W0;
      This.Observer_Velocity.Z := 0.0;
   end Initialize_Observer;

   --------------------------
   -- Initialize_Satellite --
   --------------------------

   procedure Initialize_Satellite
     (This		: in out Satellite;
      Epoch_Year	: in     Integer;
      Epoch_Time	: in     Long_Float;
      Inclination	: in     Long_Float;
      RAAN		: in     Long_Float;
      Eccentricity	: in     Long_Float;
      Argument_Perigee	: in     Long_Float;
      Mean_Anomaly	: in     Long_Float;
      Mean_Motion	: in     Long_Float;
      Decay_Rate	: in     Long_Float;
      Orbit_Number	: in     Long_Integer;
      Attitude_Longitude: in     Long_Float;
      Attitude_Latitude : in     Long_Float)
   is
   begin
      This.Epoch_Year := Epoch_Year;
      This.Time_Epoch := Epoch_Time;
      This.Eccentricity := Eccentricity;
      This.Orbit_Number := Orbit_Number;

      --  Convert Keplerian Elements into correct units
      This.Argument_Perigee := Radians (Argument_Perigee);
      This.Attitude_Latitude := Radians (Attitude_Latitude);
      This.Attitude_Longitude := Radians (Attitude_Longitude);
      This.Inclination := Radians (Inclination);
      This.Mean_Anomaly := Radians (Mean_Anomaly);
      This.RAAN := Radians (RAAN);

      This.Decay_Rate := 2.0 * Pi * Decay_Rate;
      This.Mean_Motion := 2.0 * Pi * Mean_Motion;
      This.CI := Cos (This.Inclination);
      This.SI := Sin (This.Inclination);

      --  Calculate Orbital Axes and Precession Rates
      This.N0 := This.Mean_Motion / 86400.0;	--  Mean Motion (Radians/Second)
      This.Semi_Major_Axis := (GM / This.N0 / This.N0) ** One_Third;
      This.Semi_Minor_Axis :=
        This.Semi_Major_Axis * Sqrt (1.0 - (This.Eccentricity * This.Eccentricity));

      This.Precession_Const :=
        RE * This.Semi_Major_Axis / (This.Semi_Minor_Axis * This.Semi_Minor_Axis);
      This.Precession_Const :=
        1.5 * J2 * This.Precession_Const * This.Precession_Const * This.Mean_Motion;

      This.Node_Precession := -This.Precession_Const * This.CI;
      This.Perigee_Precession :=
        This.Precession_Const * (5.0 * This.CI * This.CI - 1.0) / 2.0;
      This.Drag_Coefficient := -2.0 * This.Decay_Rate / This.Mean_Motion / 3.0;

      --  Antenna Unit Vector (ORBITAL PLANE)
      declare
         CO, CL, SO, SL	: Long_Float;
      begin
         CO := Cos (This.Attitude_Longitude);
         CL := Cos (This.Attitude_Latitude);
         SO := Sin (This.Attitude_Longitude);
         SL := Sin (This.Attitude_Latitude);
         This.Unit_Antenna.X := -CL * CO;
         This.Unit_Antenna.Y := -CL * SO;
         This.Unit_Antenna.Z := -SL;
      end;

      --  Convert Satellite Epoch to Day Number and Fraction of a Day
      This.Epoch_Day :=
        Date_To_Day (Year => This.Epoch_Year, Month => 1, Day => 0) +
        Long_Integer (Long_Float'Floor (This.Time_Epoch));
      This.Epoch_Time := This.Time_Epoch - Long_Float'Floor (This.Time_Epoch);

      --  Convert Solar Data to Satellite Epoch
      This.TEG :=
        Long_Float (This.Epoch_Day - Date_To_Day (Year => YG, Month => 1, Day => 0)) +
        This.Epoch_Time;
      This.GHAE := Radians (G0) + This.TEG * WE;
      This.MRSE := Radians (G0) + (This.TEG * WW) + Pi;
      This.MASE := Radians (MAS0 + MASD * This.TEG);
   end Initialize_Satellite;

   -------------
   -- Radians --
   -------------

   function Radians (Degrees : in Long_Float) return Long_Float is
   begin
      return ((Ada.Numerics.Pi / 180.0) * Degrees);
   end Radians;

   ---------
   -- Sgn --
   ---------

   function Sgn (This : in Long_Float) return Long_Float is
   begin
      if This < 0.0 then
         return -1.0;
      elsif This = 0.0 then
         return 0.0;
      else
         return 1.0;
      end if;
   end Sgn;

   --------------------------
   -- Time_To_Day_Fraction --
   --------------------------

   function Time_To_Day_Fraction
     (Hours		: in Integer;
      Minutes		: in Integer;
      Seconds		: in Integer) return Long_Float
   is
      DF		: Long_Float;
   begin
      DF := (Long_Float (Hours) + (Long_Float (Minutes) / 60.0) +
             (Long_Float (Seconds) / 3600.0)) / 24.0;
      return DF;
   end Time_To_Day_Fraction;

end EarthStation.Predict;

