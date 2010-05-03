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
--  earthstation-predict.ads	jvinters	2-May-2010
--

pragma License (GPL);

with Ada.Calendar;
with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
with Generic_3d_Vectors;

package EarthStation.Predict is

   package Vector_3d is new Generic_3d_Vectors (Long_Float);
   use Vector_3d;

   type Eclipse_Status is (SHADE_SIDE, SUN_SIDE, ECLIPSED, VISIBLE);

   type Observer is private;
   type Range_Vectors is private;
   type Satellite is private;
   type Satellite_Vectors is private;

   type Keplerian_Elements is record
      Epoch_Year	: Integer;
      Epoch_Time	: Long_Float;
      Inclination	: Long_Float;
      RAAN		: Long_Float;
      Eccentricity	: Long_Float;
      Argument_Perigee	: Long_Float;
      Mean_Anomaly	: Long_Float;
      Mean_Motion	: Long_Float;
      Decay_Rate	: Long_Float;
      Orbit_Number	: Long_Integer;
      ALON		: Long_Float;
      ALAT		: Long_Float;
   end record;

   RE	: constant Long_Float := 6378.137;	--  WGS-84 Earth Ellipsoid

   function Atn
     (Y			: Long_Float;
      X			: Long_Float) return Long_Float;
   --  Arctangent, protected against divide by 0

   function Calculate_Range_Vectors
     (Groundstation	: in     Observer;
      Sat_Vectors	: in     Satellite_Vectors) return Range_Vectors;
   --  Calculates Range Vectors given Satellite and Observer Information.

   function Calculate_Satellite_Vectors
     (This		: in     Satellite;
      Day_Number	: in     Long_Integer;
      Day_Fraction	: in     Long_Float) return Satellite_Vectors;
   --  Calculates Satellite (and sun) Vectors at given Day and Time.
   --  You MUST have called Initialize_Satellite once on 'This' before
   --  calling this routine (to initialize Keplerian Elements).

   procedure Clock_To_Day
     (Timestamp		: in     Ada.Calendar.Time;
      Day_Number	:    out Long_Integer;
      Day_Fraction	:    out Long_Float);
   --  Converts an Ada.Calendar.Time into Day Number and Day Fraction suitable
   --  for use by the prediction routines

   function Date_To_Day
     (Year		: in Integer;
      Month		: in Integer;
      Day		: in Integer) return Long_Integer;
   --  Returns Day number given Date

   function Degrees (Radians : in Long_Float) return Long_Float;
   --  Converts radians to degrees

   function Get_AMSAT_Day (This : in Satellite_Vectors) return Long_Integer;
   pragma Inline (Get_AMSAT_Day);
   --  Returns AMSAT Day (0 = 1-Jan-1978)

   function Get_Azimuth (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Azimuth);
   --  Returns Satellite Azimuth

   function Get_Doppler_Factor (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Doppler_Factor);
   --  Returns Satellite Doppler Factor (used for calculating Doppler Shift)

   function Get_Eclipse_Status (This : in Satellite_Vectors) return Eclipse_Status;
   pragma Inline (Get_Eclipse_Status);
   --  Returns Satellite Eclipse Status

   function Get_Elevation (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Elevation);
   --  Returns Satellite Elevation

   function Get_Height (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_Height);
   --  Returns Satellite Height

   function Get_Illumination (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_Illumination);
   --  Returns Satellite Illumination

   function Get_Latitude (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Latitude);
   --  Returns Sub-Satellite Point Latitude

   function Get_Longitude (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Longitude);
   --  Returns Sub-Satellite Point Longitude

   function Get_Orbit (This : in Satellite_Vectors) return Long_Integer;
   pragma Inline (Get_Orbit);
   --  Returns Satellite Orbit Number

   function Get_Range (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Range);
   --  Returns Satellite Range

   function Get_Range_Rate (This : in Range_Vectors) return Long_Float;
   pragma Inline (Get_Range_Rate);
   --  Returns Satellite Range Rate

   function Get_RS (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_RS);
   --  Gets Distance RS

   function Get_Sun_Azimuth (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_Sun_Azimuth);
   --  Returns Sun Azimuth

   function Get_Sun_Elevation (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_Sun_Elevation);
   --  Returns Sun Elevation

   function Get_Time_Since_Epoch (This : in Satellite_Vectors) return Long_Float;
   pragma Inline (Get_Time_Since_Epoch);

   function Get_Visible (This : in Range_Vectors) return Boolean;
   pragma Inline (Get_Visible);
   --  Returns Satellite Visibility

   function Get_Visibility (This : in Satellite_Vectors) return Eclipse_Status;
   pragma Inline (Get_Visibility);
   --  Returns Satellite Visibility/Eclipse Status

   procedure Initialize_Observer
     (This		: in out Observer;
      Latitude		: in     Long_Float;
      Longitude		: in     Long_Float;
      Height		: in     Long_Float);
   --  Initializes an Observer.
   --  Latitude and Longitude are the station location in Degrees.
   --  Height is the station height in metres.

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
      Attitude_Latitude : in     Long_Float);
   --  Initializes a Satellite.
   --  Inclination, RAAN, Argument_Perigee, Mean_Anomaly, Attitude_Longitude
   --  and Attitude_Latitude are in Degrees.

   procedure Initialize_Satellite
     (This		: in out Satellite;
      Elements		: in     Keplerian_Elements);
   --  Initializes a Satellite using preloaded Keplerian Elements

   function Radians (Degrees : in Long_Float) return Long_Float;
   --  Converts degrees to radians

   function Time_To_Day_Fraction
     (Hours		: in Integer;
      Minutes		: in Integer;
      Seconds		: in Integer) return Long_Float;
   --  Converts Time to
private

   use Ada.Numerics;
   use Ada.Numerics.Long_Elementary_Functions;

   DATE_EPOCH	: constant Integer := -428;	--  General Day Epoch

   GM	: constant Long_Float := 3.986E5;		--  Earth Grav. Constant
   J2	: constant Long_Float := 1.08263E-3;	--  2nd Zonal Coefficient
   FL   : constant Long_Float := 1.0 / 298.257224;	--  Earth Flattening
   RP	: constant Long_Float := RE * (1.0 - FL);
   XX	: constant Long_Float := RE * RE;
   ZZ	: constant Long_Float := RP * RP;
   YT	: constant Long_Float := 365.2421874;	--  Tropical Year in Days
   WW	: constant Long_Float := 2.0 * Pi / YT;	--  Earth Rot. Rate Rads/Whole Day
   WE	: constant Long_Float := 2.0 * Pi + WW;	--  Earth Rot. Rate Rads/Day
   W0	: constant Long_Float := WE / 86400.0;	--  Earth Rot. Rate Rads/Second

   --  Sun Data

   YG	: constant Integer := 2000;
   G0	: constant Long_Float := 98.9821;
   MAS0	: constant Long_Float := 356.0507;	--  Mean Anomaly Degrees
   MASD	: constant Long_Float := 0.98560028; --  Mean Anomaly Degrees/Day
   INS	: constant Long_Float := 0.40909296; --  Inclination Rads
   CNS	: constant Long_Float := Cos (INS);	--  Cosine Inclination
   SNS	: constant Long_Float := Sin (INS);	--  Sine Inclination
   EQC1	: constant Long_Float := 0.03342;	--  Equation of Centre Term
   EQC2	: constant Long_Float := 0.00035;	--  Equation of Centre Term


   type Observer is record
      Latitude		: Long_Float;		--  Observer Latitude (Rads)
      Longitude		: Long_Float;		--  Observer Longitude (Rads)
      Height		: Long_Float;		--  Observer Height (Km)
      CL		: Long_Float;		--  Cosine Latitude
      CO		: Long_Float;		--  Cosine Longitude
      D			: Long_Float;
      Rx		: Long_Float;
      Rz		: Long_Float;
      SL		: Long_Float;		--  Sine Latitude
      SO		: Long_Float;		--  Sine Longitude
      Observer_Surface	: Vector;		--  Observer XYZ Surface Coordinates
      Observer_Velocity	: Vector;		--  Observer Velocity (GEOCENTRIC)
      Unit_East		: Vector;		--  Unit East Vector (GEOCENTRIC)
      Unit_North	: Vector;		--  Unit North Vector (GEOCENTRIC)
      Unit_Up		: Vector;		--  Unit Up Vector (GEOCENTRIC)
   end record;

   type Range_Vectors is record
      Azimuth		: Long_Float;		--  Satellite Azimuth
      Doppler_Factor	: Long_Float;
      E			: Long_Float;
      Elevation		: Long_Float;		--  Satellite Elevation
      N			: Long_Float;
      Range_Magnitude	: Long_Float;
      Range_Rate	: Long_Float;
      Range_Vector	: Vector;
      Subsat_Longitude	: Long_Float;		--  Subsatellite Longitude (SLON)
      Subsat_Latitude	: Long_Float;		--  Subsatellite Latitude (SLAT)
      U			: Long_Float;
      Visible		: Boolean;		--  Satellite is Visible?
   end record;

   type Satellite is record
      Epoch_Year	: Integer;		--  Satellite Epoch Year
      Time_Epoch	: Long_Float;		--  Epoch Time in Days
      Epoch_Day		: Long_Integer;	--  Satellite Epoch as Day Number
      Epoch_Time	: Long_Float;		--  Satellite Epoch Day Fraction
      Inclination	: Long_Float;		--  Inclination (Rads)
      RAAN		: Long_Float;		--  RAAN (Rads)
      Eccentricity	: Long_Float;		--  Eccentricity
      Argument_Perigee	: Long_Float;		--  Argument of Perigee (Rads)
      Mean_Anomaly	: Long_Float;		--  Mean Anomaly (Rads)
      Mean_Motion	: Long_Float;		--  Mean Motion Rads / Day
      Decay_Rate	: Long_Float;		--  Decay Rate Rads / Day / Day
      Orbit_Number	: Long_Integer;	--  Orbit Number
      Attitude_Longitude: Long_Float;		--  Satellite Attitude (ALON, Rads)
      Attitude_Latitude	: Long_Float;		--  Satellite Attitude (ALAT, Rads)
                                       --
      CI		: Long_Float;		--  Cosine Inclination
      Drag_Coefficient	: Long_Float;		--  Drag Coefficient
      GHAE		: Long_Float;		--  GHA Aries at Epoch
      MASE		: Long_Float;		--  Mean MA of Sun
      MRSE		: Long_Float;		--  Mean RA of Sun at Sat Epoch
      N0		: Long_Float;		--  Mean Motion (Rads/Sec)
      Node_Precession	: Long_Float;		--  Node Precession Rate (Rads/Day)
      Perigee_Precession: Long_Float;		--  Perigee Precession Rate (Rads/Day)
      Precession_Const	: Long_Float;		--  Precession Constant (Rads/Day)
      Semi_Major_Axis	: Long_Float;		--  Orbit Semi Major Axis (Km)
      Semi_Minor_Axis	: Long_Float;		--  Orbit Semi Minor Axis (Km)
      SI		: Long_Float;		--  Sine Inclination
      TEG		: Long_Float;		--  Sun Data Elapsed Time
      Unit_Antenna	: Vector;		--  Antenna Unit Vector
   end record;

   --  Note: if tracking more than one satellite, this is duplication of the
   --        Sun data (GHAE/MASE/MRSE etc).  It is simpler though, to lump this
   --        in with the satellite data (and the overhead is only small anyway).

   type Satellite_Vectors is record
      A			: Long_Float;
      ANT		: Vector;		--  Antenna Axis Unit Vector (CELESTIAL)
      ALG		: Vector;		--  Antenna Axis Vector (GEOCENTRIC)
      AP		: Long_Float;
      B			: Long_Float;
      CQ		: Long_Float;
      CUA		: Long_Float;		--  Cosine Umbral Angle
      CW		: Long_Float;
      CX, CY, CZ	: Vector;
      DN		: Long_Integer;	--  Day Number
      DR		: Long_Integer;	--  Whole number of revs
      DT		: Long_Float;		--  Drag Terms
      GHAA		: Long_Float;
      H			: Vector;
      Illumination	: Long_Float;		--  Satellite Illumination
      KD		: Long_Float;		--  Drag Terms
      KDP		: Long_Float;		--  Drag Terms
      M			: Long_Float;		--  Mean Anomaly at YR, TN
      MAS		: Long_Float;
      RAAN		: Long_Float;
      RN		: Long_Integer;	--  Current Orbit Number
      RS		: Long_Float;
      SAT		: Vector;		--  Satellite Position Vector (CELESTIAL)
      SSA		: Long_Float;		--  Sine of Sun Angle
      STG		: Vector;		--  Satellite Position Vector (GEOCENTRIC)
      SUN		: Vector;		--  Sun Unit Vector (CELESTIAL)
      SUO		: Vector;		--  Sun Unit Vector (ORBITAL)
      Sun_Azimuth	: Long_Float;
      Sun_Elevation	: Long_Float;
      Sx, Sy		: Long_Float;		--  Satellite Pos in Orb. Plane
      SQ		: Long_Float;
      SW		: Long_Float;
      TAS		: Long_Float;
      Time_Since_Epoch	: Long_Float;		--  Elapsed Time Since Epoch
      UMD		: Long_Float;		--  Umbral Distance in Earth Radii
      VEL		: Vector;		--  Satellite Velocity (CELESTIAL)
      Visibility	: Eclipse_Status;
      VLG		: Vector;		--  Satellite Velocity (GEOCENTRIC)
      Vx, Vy		: Long_Float;		--  Satellite Vel in Orb. Plane
   end record;

   function Sgn (This : in Long_Float) return Long_Float;

end EarthStation.Predict;

