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
--  5. Master PIN Integrity
--     Claim:
--       The Master PIN can only be modified by the lock command in unlocked state.
--       All other operations preserve the Master PIN's value.
--     Enforcement:
--       - Handle_Lock's post-condition enforces that Master PIN is only changed when
--         the calculator is unlocked and a valid new PIN is provided:
--         Post => (if PIN_Str'Length = 4 and 
--                 (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') and
--                 Is_Unlocked(C'Old) then
--                   (Is_Locked(C) and Get_Master_PIN(C) = PIN.From_String(PIN_Str))
--                 else Get_State(C) = Get_State(C'Old) and Get_Master_PIN(C) = Get_Master_PIN(C'Old))
--
--       - All other operations (including Handle_Unlock) explicitly preserve Master PIN:
--         Post => ... and Get_Master_PIN(C) = Get_Master_PIN(C'Old)
--
--       SPARK formally proves this critical security property, ensuring the PIN
--       credential cannot be tampered with outside the authorized lock command.
--
--  6. Stack and Memory Integrity on Error
--     Claim:
--       When operations fail due to invalid conditions (e.g., stack underflow,
--       undefined memory locations), the calculator preserves its state.
--     Enforcement:
--       - Handle_Pop includes protection against stack underflow:
--         Post => (if Is_Unlocked(C'Old) and not Stack_Has(C'Old, 1) 
--                 then C.Stack_Top = C'Old.Stack_Top)
--
--       - Handle_LoadFrom prevents stack changes when accessing undefined memory:
--         Post => (if not MemoryStore.Has(C'Old.Mem, Location_Index(...)) 
--                 then C.Stack_Top = C'Old.Stack_Top)
--
--       These post-conditions formally verify that error conditions are handled
--       safely, preventing state corruption or information leakage.
--
--  SPARK Annotation for Security Properties:
--  SPARK uses flow analysis and formal verification to check that:
--   - The Is_Unlocked(C) checks in operation handlers create a barrier that
--     prevents state changes or data access when locked
--   - The conditional state transitions in Handle_Lock and Handle_Unlock ensure
--     proper authentication is required for state changes
--   - Master PIN value is preserved across all operations except authorized Lock
--   - Error conditions (stack underflow, undefined memory) maintain system integrity
--
--  These properties align with the security requirements specified in the assignment
--  and go beyond by ensuring formal verification of additional critical properties.

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
