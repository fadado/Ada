-- gotcha.ads

with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;

use Ada;

package Gotcha is

   Halt : exception;
   -- On client:
   --    raise Gotcha.Halt
   -- On Main:
   --    exception
   --       when Gotcha.Halt => null;

   procedure Report_Exception(X: Exceptions.EXCEPTION_OCCURRENCE; S: STRING);

   procedure Die;

   protected Pass is
      procedure Handle (
         Cause: Task_Termination.CAUSE_OF_TERMINATION;
         T    : Task_Identification.TASK_ID;
         X    : Exceptions.EXCEPTION_OCCURRENCE);
   end Pass; 

   procedure Set_Handlers;

end Gotcha;

-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada