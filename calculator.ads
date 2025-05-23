with MemoryStore;
with PIN;

package Calculator with SPARK_Mode is
   -- Calculator state type definition
   type Calculator_State is (Locked, Unlocked);
   
   -- Maximum stack size as specified in requirements
   Stack_Size : constant := 512;
   
   -- Calculator abstract data type
   type Calculator_Type is private;
   
   -- Initialize calculator with a master PIN
   procedure Init(C : out Calculator_Type; Master_PIN : in PIN.PIN) with
     Post => Get_State(C) = Locked;
   
   -- Get the current state of the calculator
   function Get_State(C : Calculator_Type) return Calculator_State;
   
   -- Get the master PIN of the calculator
   function Get_Master_PIN(C : Calculator_Type) return PIN.PIN with
     Pre => True;
   
   -- Execute a command on the calculator
   -- This is the main interface to interact with the calculator
   procedure Execute_Command(C : in out Calculator_Type; Command : in String);
   
   -- Exception to be raised when the calculator should exit
   Calculator_Exit_Exception : exception;
   
   -- Security-related contract checking functions
   -- These functions are used to prove security properties
   
   -- Check if the calculator is in the unlocked state
   function Is_Unlocked(C : Calculator_Type) return Boolean is
     (Get_State(C) = Unlocked);
   
   -- Check if the calculator is in the locked state
   function Is_Locked(C : Calculator_Type) return Boolean is
     (Get_State(C) = Locked);
   
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
     (C.Stack_Top + N <= Stack_Size);
   
end Calculator; 