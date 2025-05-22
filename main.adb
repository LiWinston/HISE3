pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;


procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S    : Lines.MyString;

   --  Memory database demo
   Mem  : MemoryStore.Database;
   Loc1 : MemoryStore.Location_Index := 10;
   --  PIN demo
   PIN1 : PIN.PIN := PIN.From_String ("1234");
   PIN2 : PIN.PIN := PIN.From_String ("1234");
begin
   ------------------------------------------------------------------
   --  Command-line echo
   ------------------------------------------------------------------
   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;
   
   ------------------------------------------------------------------
   --  MemoryStore CRUD(Create, Read, Update, Delete) demo
   ------------------------------------------------------------------

   MemoryStore.Init (Mem);

   Put_Line ("Storing 50 at location 10 ...");
   MemoryStore.Put (Mem, Loc1, 50);

   Put ("Location 10 now holds: ");
   Ada.Integer_Text_IO.Put (Integer (MemoryStore.Get (Mem, Loc1)), 0);
   New_Line;

   Put_Line ("Listing defined locations:");
   MemoryStore.Print (Mem);

   Put_Line ("Removing location 10 ...");
   MemoryStore.Remove (Mem, Loc1);

   if MemoryStore.Has (Mem, Loc1) then
      Put_Line ("Location 10 is still defined! (unexpected)");
   else
      Put_Line ("Location 10 successfully removed.");
   end if;
   
   ------------------------------------------------------------------
   --  Tokeniser demo
   ------------------------------------------------------------------
   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 5 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      for I in 1..NumTokens loop
         declare
            TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
         begin
            Put("Token "); Put(I); Put(" is: """);
            Put(TokStr); Put_Line("""");
         end;
      end loop;
      if NumTokens > 3 then
         Put_Line("You entered too many tokens --- I said at most 3");
      end if;
   end;
   
   ------------------------------------------------------------------
   --  PIN equality demo
   ------------------------------------------------------------------
   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
   
   ------------------------------------------------------------------
   --  32-bit overflow / parsing demo (unchanged)
   ------------------------------------------------------------------
   declare
      Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
      R : Long_Long_Integer := 
        Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   begin
      Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
      Put(Smallest_Integer); New_Line;
      
      if R < Long_Long_Integer(Integer'First) or
         R > Long_Long_Integer(Integer'Last) then
         Put_Line("Overflow would occur when trying to compute the square of this number");
      end if;
         
   end;
   Put_Line("2 ** 32 is too big to fit into an Integer...");
   Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   Put(StringToInteger.From_String("2147483648")); New_Line;
   
      
end Main;
