---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------
package body StringToInteger with SPARK_MOde Is
   function From_String(S : in String) return Integer is
      Result : Integer := 0;
      C : Integer;
      Negate : Boolean := False;
      Pos : Integer;
   begin
      if S'Length <= 0 then
         return 0;
      end if;
      Pos := S'First;
      if S(Pos) = '-' then
         Negate := True;
         if S'Length <= 1 then
            return 0;
         end if;
         Pos := S'First + 1;
      end if;
      
      
      while Pos <= S'Last loop
         pragma Loop_Invariant (Pos >= S'First and Pos <= S'Last and 
                                (if Negate then Result <= 0 else Result >= 0));
         
         if Integer'Last / 10 < Result then            
            return 0;
         end if;
         if Integer'First / 10 > Result then
            return 0;
         end if;
         Result := Result * 10;
         if S(Pos) >= '0' and S(Pos) <= '9' then
            C := Character'Pos(S(Pos)) - Character'Pos('0');
            if Negate then
               if Integer'First + C > Result then
                  return 0;
               else
                  Result := Result - C;
               end if;
            else
               if Integer'Last - C < Result then
                  return 0;
               else
                  Result := Result + C;
               end if;
            end if;
         else
            return 0;
         end if;
         if (Pos < S'Last) then
            Pos := Pos + 1;
         else
            return Result;
         end if;
      end loop;
      -- won't ever get here but keep SPARK Prover happy
      return Result;
   end From_String;
   
   function Is_Valid(S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;

      if S(S'First) = '-' then
         if S'Length = 1 then
            return False;
         end if;
         for I in S'First + 1 .. S'Last loop
            if S(I) < '0' or else S(I) > '9' then
               return False;
            end if;
         end loop;
      else
         for I in S'Range loop
            if S(I) < '0' or else S(I) > '9' then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Is_Valid;
   
end StringToInteger;   
