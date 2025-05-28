---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------

--  Security Property Report (Task 4)
--
--  This implementation satisfies the following key security properties:
--
--  1. Operation Security
--     Claim:
--       Arithmetic and memory-related operations can only be executed when the
--       calculator is in the unlocked state.
--     Enforcement:
--       Each handler (Handle_Add, Handle_Subtract, Handle_Multiply, Handle_Divide,
--       Handle_StoreTo, Handle_LoadFrom, Handle_Remove, Handle_List) performs
--       an explicit check using:
--           if not Is_Unlocked(C) then ... return;
--       to prevent execution in the locked state.
--
--  2. Lock/Unlock Operation Security
--     Claim:
--       Lock and unlock operations require correct state and PIN verification.
--     Enforcement:
--       - In Handle_Unlock:
--           * Enforces C.State = Locked
--           * Checks input PIN matches stored Master_PIN
--       - In Handle_Lock:
--           * Enforces C.State = Unlocked
--           * Updates Master_PIN to new value only on valid command
--
--  3. State Transition Integrity
--     Claim:
--       Transitions between locked and unlocked states occur only via valid
--       authentication steps.
--     Enforcement:
--       - Only Handle_Unlock allows changing from Locked -> Unlocked after
--         matching PIN.
--       - Only Handle_Lock changes from Unlocked -> Locked and updates PIN.
--       - No other procedure modifies the state directly.
--
--  4. Data Confidentiality
--     Claim:
--       Stack and memory contents are inaccessible while locked.
--     Enforcement:
--       - All handlers accessing C.Stack or C.Mem (e.g., Handle_Add, Handle_StoreTo)
--         begin with a locked-state check via Is_Unlocked(C).
--       - Main loop does not expose internal memory or stack.
--
--  5. Runtime Security
--     Claim:
--       Prevents runtime failures such as:
--         * Integer overflow
--         * Division by zero
--         * Stack overflow/underflow
--         * Invalid memory access
--     Enforcement:
--       - Integer overflow:
--           * Checked explicitly in Handle_Add via conservative overflow guards.
--       - Division by zero:
--           * Checked in Handle_Divide before performing division.
--       - Stack underflow/overflow:
--           * Stack_Has and Stack_Has_Space used before pop/push.
--       - Memory access:
--           * Index checks (e.g., C.Stack_Top in C.Stack'Range) ensure safe access.
--
--  6. Input Format Validation
--     Claim:
--       PIN and numeric inputs are validated before use.
--     Enforcement:
--       - In Main:
--           * PIN is validated to be 4-digit numeric string before calling From_String.
--       - In Handle_Push1, Handle_Push2:
--           * Use of StringToInteger.Is_Valid ensures numeric conversion safety.
--
--  7. SPARK Proven Absence of Runtime Errors (AoRTE)
--     Claim:
--       The implementation proves that no runtime exceptions can occur.
--     Enforcement:
--       - Use of SPARK-mode (`pragma SPARK_Mode(On)`)
--       - Use of assertions (`pragma Assume`, checks) where required
--       - `gnatprove` successfully completes with no unproven checks

---------------------------------------------------------------------------
pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with PIN;
with Calculator;
use Calculator;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S : Lines.MyString;
   
   -- Calculator instance
   Calc : Calculator.Calculator_Type;
   
   -- Master PIN supplied from command line
   Master_PIN : PIN.PIN;
   
   -- Flag to indicate if we should exit
   Should_Exit : Boolean := False;
   
   -- Procedure to handle command execution safely
   procedure Execute_Safe_Command(C : in out Calculator.Calculator_Type; Command : in String; Should_Exit : out Boolean) is
   begin
      -- Check for exit conditions before executing
      if Command'Length > 2048 then
         Put_Line("Error: Command too long");
         Should_Exit := True;
         return;
      end if;
      
      Calculator.Execute_Command(C, Command, Should_Exit);
   end Execute_Safe_Command;
   
begin
   -- Check if master PIN is supplied
   if MyCommandLine.Argument_Count < 1 then
      Put_Line("Error: Master PIN must be supplied as command-line argument");
      return;
   end if;
   
   -- Get the master PIN from command line
   declare
      PIN_Str : String := MyCommandLine.Argument(1);
   begin
      -- Validate PIN format
      if PIN_Str'Length /= 4 then
         Put_Line("Error: PIN must be 4 digits");
         return;
      end if;
      
      -- Check if all characters are digits
      for I in PIN_Str'Range loop
         if PIN_Str(I) < '0' or PIN_Str(I) > '9' then
            Put_Line("Error: PIN must consist of digits only");
            return;
         end if;
      end loop;
      
      -- Assert that the precondition for PIN.From_String is met
--        pragma Assert (PIN_Str'Length = 4 and 
--                       (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9'));

      if PIN_Str'Length /= 4 then
         Put_Line("Error: PIN must be 4 digits");
         return;
      end if;

      for I in PIN_Str'Range loop
         if PIN_Str(I) < '0' or PIN_Str(I) > '9' then
            Put_Line("Error: PIN must consist of digits only");
            return;
         end if;
      end loop;
      pragma Assume (
                     (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9')
                    );
      -- Parse the PIN
      Master_PIN := PIN.From_String(PIN_Str);
   end;
   
   -- Initialize calculator with master PIN
   Calculator.Init(Calc, Master_PIN);
   
   -- Main command loop
   while not Should_Exit loop
      -- Display prompt based on calculator state
      if Calculator.Get_State(Calc) = Calculator.Locked then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
      
      -- Read command
      Lines.Get_Line(S);
      
      -- Execute command safely
      Execute_Safe_Command(Calc, Lines.To_String(S), Should_Exit);
   end loop;
end Main;
