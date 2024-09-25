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
      msg : STRING := Exception_Message(X);
   begin
      Put_Line(Standard_Error, S);
      Put_Line(Standard_Error, Exception_Name(X));
      Put_Line(Standard_Error, Exception_Information(X));
      if msg /= "" then
         Put_Line(Standard_Error, msg);
      end if;
   end Report_Exception;

   procedure Die is
   begin
      Abort_Task(Current_Task);
   end Die;

   protected body Pass is
      procedure Handle (
         Cause: Task_Termination.Cause_Of_Termination;
         T    : Task_Identification.Task_Id;
         X    : Exceptions.Exception_Occurrence)
      is
         message : STRING := Exception_Message(X);
      begin
         case Cause is
            when Normal => null;
            when Abnormal =>
               Put(Standard_Error, "Something nasty happened to task ");
               Put_Line(Standard_Error, Image(T));
            when Unhandled_Exception =>
               Report_Exception(X, Image(T));
         end case;
      end Handle;
   end Pass;

   procedure Set_Handlers is
   begin
      Set_Specific_Handler(Current_Task, Pass.Handle'Access);
      Set_Dependents_Fallback_Handler(Pass.Handle'Access);
   end Set_Handlers;

end Gotcha;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
