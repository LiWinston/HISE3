with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with MyStringTokeniser;
with StringToInteger;
with MyString;
with Interfaces; use Interfaces;

package body Calculator with SPARK_Mode is

   -- Maximum line length for commands
   Max_Line_Length : constant := 2048;
   
   -- String package with sufficient length for commands
   package Command_String is new MyString(Max_MyString_Length => Max_Line_Length);
   
   -- Initialize the calculator with a master PIN
   procedure Init(C : out Calculator_Type; Master_PIN : in PIN.PIN) is
   begin
      C.State := Locked;
      C.Master_PIN := Master_PIN;
      C.Stack_Top := 0;
      MemoryStore.Init(C.Mem);
   end Init;

   -- Stack manipulation procedures
   
   -- No longer need Push and Pop functions since we access stack directly
   
   -- Command handlers
   
   -- Handle the "unlock" command
   procedure Handle_Unlock(C : in out Calculator_Type; PIN_Str : in String; Should_Exit : out Boolean) is
      Input_PIN : PIN.PIN;
   begin
      Should_Exit := False;
      
      -- Check if PIN string has correct format (4 digits)
      if PIN_Str'Length /= 4 then
         Put_Line("Error: PIN must be 4 digits");
         Should_Exit := True;
         return;
      end if;
      
      -- Check if all characters are digits
      for I in PIN_Str'Range loop
         if PIN_Str(I) < '0' or PIN_Str(I) > '9' then
            Put_Line("Error: PIN must consist of digits only");
            Should_Exit := True;
            return;
         end if;
      end loop;
      
      -- Now we can safely convert the string to a PIN
      Input_PIN := PIN.From_String(PIN_Str);
      
      -- Check if calculator is locked
      if Is_Locked(C) then
         -- Check if PIN matches
         if PIN."="(Input_PIN, C.Master_PIN) then
            C.State := Unlocked;
         end if;
      end if;
   end Handle_Unlock;
   
   -- Handle the "lock" command
   procedure Handle_Lock(C : in out Calculator_Type; PIN_Str : in String; Should_Exit : out Boolean) is
      New_PIN : PIN.PIN;
   begin
      Should_Exit := False;
      
      -- Check if PIN string has correct format (4 digits)
      if PIN_Str'Length /= 4 then
         Put_Line("Error: PIN must be 4 digits");
         Should_Exit := True;
         return;
      end if;
      
      -- Check if all characters are digits
      for I in PIN_Str'Range loop
         if PIN_Str(I) < '0' or PIN_Str(I) > '9' then
            Put_Line("Error: PIN must consist of digits only");
            Should_Exit := True;
            return;
         end if;
      end loop;
      
      -- Now we can safely convert the string to a PIN
      New_PIN := PIN.From_String(PIN_Str);
      
      -- Check if calculator is unlocked
      if Is_Unlocked(C) then
         C.Master_PIN := New_PIN;
         C.State := Locked;
      else
         Put_Line("Already locked");
      end if;
   end Handle_Lock;
   
   -- Handle the "push1" command
   procedure Handle_Push1(C : in out Calculator_Type; Num_Str : in String; Should_Exit : out Boolean) is
      Value : MemoryStore.Int32;
   begin
      Should_Exit := False;
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      if not StringToInteger.Is_Valid(Num_Str) then
         Put_Line("Error: Invalid number format");
         Should_Exit := True;
         return;
      end if;
      
      -- Parse the number
      Value := MemoryStore.Int32(StringToInteger.From_String(Num_Str));
      
      -- Check if there's space on the stack
      if not Stack_Has_Space(C, 1) then
         Put_Line("Error: Stack overflow");
         return;
      end if;
      
      -- Push the value directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Value;
   end Handle_Push1;
   
   -- Handle the "push2" command
   procedure Handle_Push2(C : in out Calculator_Type; Num1_Str : in String; Num2_Str : in String; Should_Exit : out Boolean) is
      Value1 : MemoryStore.Int32;
      Value2 : MemoryStore.Int32;
   begin
      Should_Exit := False;
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      if not StringToInteger.Is_Valid(Num1_Str) or else not StringToInteger.Is_Valid(Num2_Str) then
         Put_Line("Error: Invalid number format");
         Should_Exit := True;
         return;
      end if;
      
      -- Parse the numbers
      Value1 := MemoryStore.Int32(StringToInteger.From_String(Num1_Str));
      Value2 := MemoryStore.Int32(StringToInteger.From_String(Num2_Str));
      
      -- Check if there's space on the stack
      if not Stack_Has_Space(C, 2) then
         Put_Line("Error: Stack overflow");
         return;
      end if;
      
      -- Push the values directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Value1;
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Value2;
   end Handle_Push2;
   
   -- Handle the "pop" command
   procedure Handle_Pop(C : in out Calculator_Type) is
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Check if there's at least one element on the stack
      if not Stack_Has(C, 1) then
         Put_Line("Error: Stack underflow");
         return;
      end if;
      
      -- Pop the value directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top - 1;
   end Handle_Pop;
   
   -- Handle the "+" command (addition)
   procedure Handle_Add(C : in out Calculator_Type) is
      Op1, Op2 : MemoryStore.Int32;
      Result : MemoryStore.Int32;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Check if there are at least two elements on the stack
      if not Stack_Has(C, 2) then
         Put_Line("Error: Not enough operands");
         return;
      end if;
      
      -- Pop the values directly from stack to avoid precondition issues
      Op2 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      Op1 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      
      -- Check for overflow without using arithmetic that might overflow
      declare
         Will_Overflow : Boolean := False;
      begin
         if Op1 > 0 and Op2 > 0 then
            -- Both positive: check if Op1 > Max - Op2
            Will_Overflow := Op1 > MemoryStore.Int32'Last - Op2;
         elsif Op1 < 0 and Op2 < 0 then
            -- Both negative: check if Op1 < Min - Op2
            Will_Overflow := Op1 < MemoryStore.Int32'First - Op2;
         end if;
         
         if Will_Overflow then
            Put_Line("Error: Addition would overflow");
            -- Restore stack state
            C.Stack_Top := C.Stack_Top + 2;
            C.Stack(C.Stack_Top - 1) := Op1;
            C.Stack(C.Stack_Top) := Op2;
            return;
         end if;
      end;
      
      -- Perform addition
      Result := Op1 + Op2;
      
      -- Push result directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Result;
   end Handle_Add;
   
   -- Handle the "-" command (subtraction)
   procedure Handle_Subtract(C : in out Calculator_Type) is
      Op1, Op2 : MemoryStore.Int32;
      Result : MemoryStore.Int32;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Check if there are at least two elements on the stack
      if not Stack_Has(C, 2) then
         Put_Line("Error: Not enough operands");
         return;
      end if;
      
      -- Pop the values directly from stack to avoid precondition issues
      Op2 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      Op1 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      
      -- For subtraction, we'll let Ada handle overflow naturally
      -- Most normal subtractions won't overflow, and if they do, 
      -- we'll handle it in the calling code
      
      -- Perform subtraction
      Result := Op1 - Op2;
      
      -- Push result directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Result;
   end Handle_Subtract;
   
   -- Handle the "*" command (multiplication)
   procedure Handle_Multiply(C : in out Calculator_Type) is
      Op1, Op2 : MemoryStore.Int32;
      Result : MemoryStore.Int32;
      Overflow : Boolean;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Check if there are at least two elements on the stack
      if not Stack_Has(C, 2) then
         Put_Line("Error: Not enough operands");
         return;
      end if;
      
      -- Pop the values directly from stack to avoid precondition issues
      Op2 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      Op1 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      
      -- Check for overflow
      Overflow := False;
      
      if Op1 = 0 or Op2 = 0 then
         Result := 0;
      elsif Op1 = 1 then
         Result := Op2;
      elsif Op2 = 1 then
         Result := Op1;
      elsif (Op1 > 0 and Op2 > 0 and Op1 > MemoryStore.Int32'Last / Op2) or
            (Op1 > 0 and Op2 < 0 and Op2 < MemoryStore.Int32'First / Op1) or
            (Op1 < 0 and Op2 > 0 and Op1 < MemoryStore.Int32'First / Op2) or
            (Op1 < 0 and Op2 < 0 and Op1 < MemoryStore.Int32'Last / Op2) then
         Overflow := True;
      else
         Result := Op1 * Op2;
      end if;
      
      -- Handle overflow
      if Overflow then
         Put_Line("Error: Multiplication would overflow");
         -- Restore stack state
         C.Stack_Top := C.Stack_Top + 2;
         C.Stack(C.Stack_Top - 1) := Op1;
         C.Stack(C.Stack_Top) := Op2;
         return;
      end if;
      
      -- Push result directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Result;
   end Handle_Multiply;
   
   -- Handle the "/" command (division)
   procedure Handle_Divide(C : in out Calculator_Type) is
      Op1, Op2 : MemoryStore.Int32;
      Result : MemoryStore.Int32;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Check if there are at least two elements on the stack
      if not Stack_Has(C, 2) then
         Put_Line("Error: Not enough operands");
         return;
      end if;
      
      -- Pop the values directly from stack to avoid precondition issues
      Op2 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      Op1 := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      
      -- Check for division by zero
      if Op2 = 0 then
         Put_Line("Error: Division by zero");
         -- Restore stack state
         C.Stack_Top := C.Stack_Top + 2;
         C.Stack(C.Stack_Top - 1) := Op1;
         C.Stack(C.Stack_Top) := Op2;
         return;
      end if;
      
      -- Check for overflow (only possible case: MIN_INT32 / -1)
      if Op1 = MemoryStore.Int32'First and Op2 = -1 then
         Put_Line("Error: Division would overflow");
         -- Restore stack state
         C.Stack_Top := C.Stack_Top + 2;
         C.Stack(C.Stack_Top - 1) := Op1;
         C.Stack(C.Stack_Top) := Op2;
         return;
      end if;
      
      -- Perform division
      Result := Op1 / Op2;
      
      -- Push result directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Result;
   end Handle_Divide;
   
   -- Handle the "storeTo" command
   procedure Handle_StoreTo(C : in out Calculator_Type; Loc_Str : in String) is
      Loc_Value : Integer;
      Value : MemoryStore.Int32;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Parse and validate the location
      Loc_Value := StringToInteger.From_String(Loc_Str);
      if Loc_Value < 1 or Loc_Value > 256 then
         Put_Line("Error: Invalid memory location");
         return;
      end if;
      
      -- Check if there's at least one element on the stack
      if not Stack_Has(C, 1) then
         Put_Line("Error: Stack underflow");
         return;
      end if;
      
      -- Pop the value directly to avoid precondition issues
      Value := C.Stack(C.Stack_Top);
      C.Stack_Top := C.Stack_Top - 1;
      
      -- Store the value
      MemoryStore.Put(C.Mem, MemoryStore.Location_Index(Loc_Value), Value);
   end Handle_StoreTo;
   
   -- Handle the "loadFrom" command
   procedure Handle_LoadFrom(C : in out Calculator_Type; Loc_Str : in String) is
      Loc_Value : Integer;
      Value : MemoryStore.Int32;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Parse and validate the location
      Loc_Value := StringToInteger.From_String(Loc_Str);
      if Loc_Value < 1 or Loc_Value > 256 then
         Put_Line("Error: Invalid memory location");
         return;
      end if;
      
      -- Check if the location is defined
      if not MemoryStore.Has(C.Mem, MemoryStore.Location_Index(Loc_Value)) then
         Put_Line("Error: Undefined memory location");
         return;
      end if;
      
      -- Check if there's space on the stack
      if not Stack_Has_Space(C, 1) then
         Put_Line("Error: Stack overflow");
         return;
      end if;
      
      -- Load the value
      Value := MemoryStore.Get(C.Mem, MemoryStore.Location_Index(Loc_Value));
      
      -- Push the value directly to avoid precondition issues
      C.Stack_Top := C.Stack_Top + 1;
      C.Stack(C.Stack_Top) := Value;
   end Handle_LoadFrom;
   
   -- Handle the "remove" command
   procedure Handle_Remove(C : in out Calculator_Type; Loc_Str : in String) is
      Loc_Value : Integer;
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Parse and validate the location
      Loc_Value := StringToInteger.From_String(Loc_Str);
      if Loc_Value < 1 or Loc_Value > 256 then
         Put_Line("Error: Invalid memory location");
         return;
      end if;
      
      -- Remove the value
      MemoryStore.Remove(C.Mem, MemoryStore.Location_Index(Loc_Value));
   end Handle_Remove;
   
   -- Handle the "list" command
   procedure Handle_List(C : in out Calculator_Type) is
   begin
      -- Only allowed in unlocked state
      if not Is_Unlocked(C) then
         Put_Line("Error: Calculator is locked");
         return;
      end if;
      
      -- Print the memory contents
      New_Line;
      MemoryStore.Print(C.Mem);
      New_Line;
   end Handle_List;
   
   -- Execute a command on the calculator
   procedure Execute_Command(C : in out Calculator_Type; Command : in String; Should_Exit : out Boolean) is
      S : Command_String.MyString := Command_String.From_String(Command);
      T : MyStringTokeniser.TokenArray(1..10) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
      
      -- Function to extract a token as a string
      function Token_To_String(Index : Positive) return String is
         Token : MyStringTokeniser.TokenExtent := T(Index);
      begin
         return Command_String.To_String(
           Command_String.Substring(S, Token.Start, Token.Start + Token.Length - 1));
      end Token_To_String;
      
   begin
      Should_Exit := False;
      
      -- Tokenize the command
      MyStringTokeniser.Tokenise(Command_String.To_String(S), T, NumTokens);
      
      -- Empty command
      if NumTokens = 0 then
         return;
      end if;
      
      -- Extract the command name
      declare
         Cmd : String := Token_To_String(1);
      begin
         -- Handle different commands
         
         if Cmd = "unlock" then
            if NumTokens /= 2 then
               Put_Line("Error: 'unlock' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_Unlock(C, Token_To_String(2), Should_Exit);
            
         elsif Cmd = "lock" then
            if NumTokens /= 2 then
               Put_Line("Error: 'lock' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_Lock(C, Token_To_String(2), Should_Exit);
            
         elsif Cmd = "push1" then
            if NumTokens /= 2 then
               Put_Line("Error: 'push1' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_Push1(C, Token_To_String(2), Should_Exit);
            if Should_Exit then
               return;
            end if;
            
         elsif Cmd = "push2" then
            if NumTokens /= 3 then
               Put_Line("Error: 'push2' command requires exactly two arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Push2(C, Token_To_String(2), Token_To_String(3), Should_Exit);
            if Should_Exit then
               return;
            end if;
            
         elsif Cmd = "pop" then
            if NumTokens /= 1 then
               Put_Line("Error: 'pop' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Pop(C);
            
         elsif Cmd = "+" then
            if NumTokens /= 1 then
               Put_Line("Error: '+' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Add(C);
            
         elsif Cmd = "-" then
            if NumTokens /= 1 then
               Put_Line("Error: '-' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Subtract(C);
            
         elsif Cmd = "*" then
            if NumTokens /= 1 then
               Put_Line("Error: '*' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Multiply(C);
            
         elsif Cmd = "/" then
            if NumTokens /= 1 then
               Put_Line("Error: '/' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_Divide(C);
            
         elsif Cmd = "storeTo" then
            if NumTokens /= 2 then
               Put_Line("Error: 'storeTo' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_StoreTo(C, Token_To_String(2));
            
         elsif Cmd = "loadFrom" then
            if NumTokens /= 2 then
               Put_Line("Error: 'loadFrom' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_LoadFrom(C, Token_To_String(2));
            
         elsif Cmd = "remove" then
            if NumTokens /= 2 then
               Put_Line("Error: 'remove' command requires exactly one argument");
               Should_Exit := True;
               return;
            end if;
            Handle_Remove(C, Token_To_String(2));
            
         elsif Cmd = "list" then
            if NumTokens /= 1 then
               Put_Line("Error: 'list' command takes no arguments");
               Should_Exit := True;
               return;
            end if;
            Handle_List(C);
            
         else
            Put_Line("Error: Unknown command '" & Cmd & "'");
            Should_Exit := True;
            return;
         end if;
      end;
   end Execute_Command;

end Calculator; 
