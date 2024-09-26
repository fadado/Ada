-- gotcha.adb

with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.Text_IO;

use Ada.Task_Identification;
use Ada.Task_Termination;
use Ada.Exceptions;
use Ada.Text_IO;

package body Gotcha is

   procedure Report_Exception(X: EXCEPTION_OCCURRENCE; S: STRING) is
      message : STRING renames Exception_Message(X);
   begin
      Put("## ");
      Put_Line(Standard_Error, S);
      Put("## Raised ");
      Put_Line(Standard_Error, Exception_Name(X));
      if message /= "" then
         Put("## ");
         Put_Line(Standard_Error, Exception_Message(X));
      end if;
   end Report_Exception;

   procedure Die is
   begin
      Abort_Task(Current_Task);
   end Die;

   protected body Pass is
      procedure Handle (
         Cause: Task_Termination.CAUSE_OF_TERMINATION;
         T    : Task_Identification.TASK_ID;
         X    : Exceptions.EXCEPTION_OCCURRENCE
      ) is
         verbose : BOOLEAN := FALSE;
      begin
         case Cause is
            when Normal =>
               if verbose then
                  Put(Standard_Error, ">> Normal termination: ");
                  Put_Line(Standard_Error, Image(T));
               end if;
            when Abnormal =>
               Put(Standard_Error, ">> Abnormal termination: ");
               Put_Line(Standard_Error, Image(T));
            when Unhandled_Exception =>
               Put(Standard_Error, ">> Unhandled Exception ");
               Put(Standard_Error, Exception_Name(X));
               Put(Standard_Error, ": ");
               Put_Line(Standard_Error, Image(T));
         end case;
      end Handle;
   end Pass;

   procedure Set_Handlers is
   begin
    -- this hangs at main end:
    --Set_Specific_Handler(Current_Task, Pass.Handle'Access);

      Set_Dependents_Fallback_Handler(Pass.Handle'Access);
   end Set_Handlers;

end Gotcha;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
