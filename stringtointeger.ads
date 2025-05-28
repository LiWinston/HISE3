---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------
package StringToInteger with SPARK_Mode Is
   
   -- returns 0 in case of invalid input
   function From_String(S : in String) return Integer;
   function Is_Valid(S : String) return Boolean
     with Pre => S'Length > 0;
end StringToInteger;
