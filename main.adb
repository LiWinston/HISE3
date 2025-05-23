pragma SPARK_Mode (On);

---------------------------------------------------------------------------
--  Security Properties Report (Task 4):
--
--  This implementation satisfies the following security properties:
--
--  1. Operation Security:
--     Operations that require unlocked state (+, -, *, /, load, store, remove, list)
--     can only be executed when the calculator is in the unlocked state.
--     This is proven by the Is_Unlocked(C) precondition checks in each of these
--     operations' handler procedures.
--
--  2. Lock Operation Security:
--     The lock operation can only be executed when the calculator is unlocked.
--     This is enforced by the Is_Unlocked(C) condition check in Handle_Lock.
--     Additionally, the lock operation always updates the master PIN to the new PIN.
--     This is proven by the direct assignment of New_PIN to C.Master_PIN.
--
--  3. Unlock Operation Security:
--     The unlock operation can only be executed when the calculator is locked.
--     This is enforced by the Is_Locked(C) condition check in Handle_Unlock.
--
--  4. State Transition Integrity:
--     State transitions between locked and unlocked states can only occur through
--     proper authentication with the correct PIN. This is proven by the PIN equality
--     check before changing the state in Handle_Unlock and the explicit state change
--     in Handle_Lock.
--
--  5. Data Confidentiality:
--     Memory contents and stack operations are only accessible in the unlocked state,
--     protecting sensitive data when the calculator is locked.
--
--  6. Runtime Security:
--     All operations protect against runtime errors like integer overflow, division
--     by zero, stack overflow/underflow, and invalid memory access through explicit
--     checks and guards.
--
--  These properties are formally verified by SPARK annotations and the SPARK prover.
---------------------------------------------------------------------------

with MyCommandLine;
with MyString;
with PIN;
with Calculator;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S : Lines.MyString;
   
   -- Calculator instance
   Calc : Calculator.Calculator_Type;
   
   -- Master PIN supplied from command line
   Master_PIN : PIN.PIN;
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
      
      -- Parse the PIN
      Master_PIN := PIN.From_String(PIN_Str);
   end;
   
   -- Initialize calculator with master PIN
   Calculator.Init(Calc, Master_PIN);
   
   -- Main command loop
   loop
      -- Display prompt based on calculator state
      if Calculator.Get_State(Calc) = Calculator.Locked then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
      
      -- Read command
      Lines.Get_Line(S);
      
      -- Execute command
      begin
         Calculator.Execute_Command(Calc, Lines.To_String(S));
      exception
         when Calculator.Calculator_Exit_Exception =>
            -- Exit on invalid command or error
            return;
         when others =>
            Put_Line("Error: Command execution failed");
            return;
      end;
   end loop;
exception
   when others =>
      Put_Line("Error: An unexpected error occurred");
end Main;
