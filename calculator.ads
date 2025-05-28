---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------
with MemoryStore;
with PIN;
use PIN;

package Calculator with SPARK_Mode is
   -- Calculator state type definition
   type Calculator_State is (Locked, Unlocked);
   
   -- Maximum stack size as specified in requirements
   Stack_Size : constant := 512;
   
   -- Calculator abstract data type
   type Calculator_Type is private;
   
   -- Initialize calculator with a master PIN
   procedure Init(C : out Calculator_Type; Master_PIN : in PIN.PIN) with
     Post => Get_State(C) = Locked and Get_Master_PIN(C) = Master_PIN;
   
   -- Get the current state of the calculator
   function Get_State(C : Calculator_Type) return Calculator_State;
   
   -- Get the master PIN of the calculator
   function Get_Master_PIN(C : Calculator_Type) return PIN.PIN with
     Pre => True;
   
   -- Execute a command on the calculator
   -- This is the main interface to interact with the calculator
   -- Should_Exit will be set to True if the calculator should exit
   procedure Execute_Command(C : in out Calculator_Type; Command : in String; Should_Exit : out Boolean);
   
   -- Security-related contract checking functions
   -- These functions are used to prove security properties
   
   -- Check if the calculator is in the unlocked state
   function Is_Unlocked(C : Calculator_Type) return Boolean is
     (Get_State(C) = Unlocked);
   
   -- Check if the calculator is in the locked state
   function Is_Locked(C : Calculator_Type) return Boolean is
     (Get_State(C) = Locked);
   
   -- Handle the "unlock" command - Security contract
   procedure Handle_Unlock(C : in out Calculator_Type; PIN_Str : in String; Should_Exit : out Boolean) with
     Pre => True,
     Post => (if PIN_Str'Length = 4 and 
              (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') and
              Is_Locked(C'Old) then
                (if PIN.From_String(PIN_Str) = Get_Master_PIN(C'Old) then
                   Is_Unlocked(C)
                 else Get_State(C) = Get_State(C'Old))
              else Get_State(C) = Get_State(C'Old));
   
   -- Handle the "lock" command - Security contract
   procedure Handle_Lock(C : in out Calculator_Type; PIN_Str : in String; Should_Exit : out Boolean) with
     Pre => True,
     Post => (if PIN_Str'Length = 4 and 
              (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') and
              Is_Unlocked(C'Old) then
                (Is_Locked(C) and Get_Master_PIN(C) = PIN.From_String(PIN_Str))
              else Get_State(C) = Get_State(C'Old));
   
   -- Handle the "+" command (addition) - Security contract
   procedure Handle_Add(C : in out Calculator_Type) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "-" command (subtraction) - Security contract
   procedure Handle_Subtract(C : in out Calculator_Type) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "*" command (multiplication) - Security contract
   procedure Handle_Multiply(C : in out Calculator_Type) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "/" command (division) - Security contract
   procedure Handle_Divide(C : in out Calculator_Type) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "storeTo" command - Security contract
   procedure Handle_StoreTo(C : in out Calculator_Type; Loc_Str : in String) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "loadFrom" command - Security contract
   procedure Handle_LoadFrom(C : in out Calculator_Type; Loc_Str : in String) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "remove" command - Security contract
   procedure Handle_Remove(C : in out Calculator_Type; Loc_Str : in String) with
     Pre => True,
     Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old));
   
   -- Handle the "list" command - Security contract
   procedure Handle_List(C : in Calculator_Type) with
     Pre => True,
     Post => True;
   
private
   -- Stack definition
   type Stack_Array is array (1..Stack_Size) of MemoryStore.Int32;
   
   -- Calculator internal structure
   type Calculator_Type is record
      State      : Calculator_State := Locked;
      Master_PIN : PIN.PIN;
      Stack      : Stack_Array;
      Stack_Top  : Natural := 0; -- 0 indicates empty stack
      Mem        : MemoryStore.Database;
   end record;
   
   -- Get the current state of the calculator
   function Get_State(C : Calculator_Type) return Calculator_State is
     (C.State);
   
   -- Get the master PIN of the calculator
   function Get_Master_PIN(C : Calculator_Type) return PIN.PIN is
     (C.Master_PIN);
   
   -- Check if the stack has enough elements for operations
   function Stack_Has(C : Calculator_Type; N : Positive) return Boolean is
     (C.Stack_Top >= N);
   
   -- Check if there is space on the stack to push more elements
   function Stack_Has_Space(C : Calculator_Type; N : Positive) return Boolean is
     (N <= Stack_Size - C.Stack_Top);
   
end Calculator; 
