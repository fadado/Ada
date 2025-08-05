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
      message     : STRING renames Exception_Message(X);
    --information : STRING renames Exception_Information(X);
   begin
      Put(Standard_Error, "## ");
      Put_Line(Standard_Error, S);
      Put(Standard_Error, "## Raised ");
      Put_Line(Standard_Error, Exception_Name(X));
      if message /= "" then
         Put(Standard_Error, "## ");
         Put_Line(Standard_Error, Exception_Message(X));
      end if;
    --if information /= "" then
    --   Put(Standard_Error, "## ");
    --   Put_Line(Standard_Error, Exception_Information(X));
    --end if;
   end Report_Exception;

   protected body Pass is
   -- warning: cannot raise exceptions inside handler!
      procedure Handle (
         Cause: Ada.Task_Termination.CAUSE_OF_TERMINATION;
         T    : Ada.Task_Identification.TASK_ID;
         X    : Ada.Exceptions.EXCEPTION_OCCURRENCE
      ) is
         verbose : BOOLEAN := FALSE;
      begin
         -- warning: this procedure output can overwrite other outputs!
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
      Set_Specific_Handler(Current_Task, Pass.Handle'Access);
      Set_Dependents_Fallback_Handler(Pass.Handle'Access);
   end Set_Handlers;

   procedure Stop is
   begin
      Abort_Task(Current_Task);
   end Stop;

end Gotcha;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
