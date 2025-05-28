---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------
--  MyCommandLine.ads
--
--  This file defines a small SPARK-compatible wrapper around Ada.Command_Line.
--  It provides a safe interface for accessing command-line arguments.
--
--  Do not modify this file.

package MyCommandLine with SPARK_Mode is

   function Argument_Count return Natural;

   function Command_Name return String;

   function Argument(Number : in Positive) return String with
     Pre => Number <= Argument_Count;

end MyCommandLine;
