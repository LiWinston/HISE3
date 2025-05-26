

package StringToInteger with SPARK_Mode Is
   
   -- returns 0 in case of invalid input
   function From_String(S : in String) return Integer;
   function Is_Valid(S : String) return Boolean
     with Pre => S'Length > 0;
end StringToInteger;
