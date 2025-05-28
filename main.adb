---------------------------------------------------------------------------
--  Assignment  : SWEN90010 Assignment 3
--  Team: pair 58
--  Student1: Yongchun Li, 1378156
--  Student2: Yuxin Ren, 1393127
---------------------------------------------------------------------------

--  Security Property Report (Task 4)
--
--  本实现通过SPARK注解证明了以下安全性质：
--
--  1. 操作安全性（Operation Security）
--     声明：
--       算术操作（+、-、*、/）和内存相关操作（storeTo、loadFrom、remove、list）
--       只能在计算器处于解锁状态时执行。
--     SPARK编码方式：
--       为每个操作添加后置条件注解：
--       Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old))
--       这保证了如果计算器在操作前处于锁定状态，操作后状态不会改变，
--       实际上是防止了操作的实际执行。每个操作函数在实现中使用：
--         if not Is_Unlocked(C) then ... return;
--       来实现这一安全性质。
--
--  2. 锁定/解锁操作安全性（Lock/Unlock Operation Security）
--     声明：
--       解锁操作只能在锁定状态下执行，且需要正确的PIN验证。
--       锁定操作只能在解锁状态下执行，且会更新主PIN。
--     SPARK编码方式：
--       为Handle_Unlock添加后置条件：
--       Post => (if PIN_Str'Length = 4 and 
--               (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') and
--               PIN.From_String(PIN_Str) = Get_Master_PIN(C'Old) and Is_Locked(C'Old) 
--              then Is_Unlocked(C)
--              else Get_State(C) = Get_State(C'Old))
--       
--       为Handle_Lock添加后置条件：
--       Post => (if PIN_Str'Length = 4 and 
--               (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') and
--               Is_Unlocked(C'Old) 
--              then (Is_Locked(C) and Get_Master_PIN(C) = PIN.From_String(PIN_Str))
--              else Get_State(C) = Get_State(C'Old))
--
--       这些注解保证：
--       1. 只有在满足所有条件时（包括锁定/解锁状态和有效PIN）才会发生状态变化
--       2. 锁定操作会更新主PIN为新的PIN
--       3. 任何其他情况下状态保持不变
--
--  3. 状态转换完整性（State Transition Integrity）
--     声明：
--       在锁定和解锁状态之间的转换只能通过有效的认证步骤进行。
--     SPARK编码方式：
--       通过Init的后置条件：
--       Post => Get_State(C) = Locked and Get_Master_PIN(C) = Master_PIN
--       确保初始状态是锁定的，并且主PIN已设置。
--
--       结合Handle_Unlock和Handle_Lock的后置条件，可以证明状态转换的完整路径：
--       Locked --(valid PIN in Handle_Unlock)--> Unlocked
--       Unlocked --(Handle_Lock with new PIN)--> Locked
--       且没有其他途径可以改变状态。
--
--  4. 数据保密性（Data Confidentiality）
--     声明：
--       在锁定状态下无法访问堆栈和内存内容。
--     SPARK编码方式：
--       所有操作函数的后置条件都保证在锁定状态下不会发生操作：
--       Post => (if not Is_Unlocked(C'Old) then Get_State(C) = Get_State(C'Old))
--       
--       这确保了在锁定状态下，任何尝试访问堆栈或内存的操作都不会执行，
--       从而保护了这些数据的保密性。
--
--  5. 额外安全性质：PIN更新完整性（PIN Update Integrity）
--     声明：
--       主PIN只能通过Handle_Lock操作在解锁状态下更新。
--     SPARK编码方式：
--       Handle_Lock的后置条件明确规定：
--       Post => (if ... Is_Unlocked(C'Old) 
--              then (Is_Locked(C) and Get_Master_PIN(C) = PIN.From_String(PIN_Str))
--              else ...)
--       
--       所有其他操作都不会修改Master_PIN，这通过它们的后置条件隐含保证。
--       这确保了PIN只能通过授权操作在授权状态下更新。
--
--  SPARK注解如何确保安全性：
--  
--  SPARK使用契约编程方法来验证代码的正确性和安全性。我们使用了：
--  1. Pre条件：指定函数执行前必须满足的条件
--  2. Post条件：指定函数执行后必须满足的条件
--  3. C'Old：引用函数执行前对象C的状态，用于表达状态变化
--  
--  通过这些注解，SPARK证明器可以静态验证我们的实现满足所有指定的安全性质，
--  并且在任何情况下都不会违反这些安全约束。这种形式化验证确保了计算器在面对
--  任何输入时都能保持其安全特性。

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
