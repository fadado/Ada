-- gotcha.ads

with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;

package Gotcha is

   Halt : exception;
   -- On subprogram:
   --    raise Gotcha.Halt
   -- On main:
   --    exception
   --       when Gotcha.Halt => null;

   procedure Report_Exception(
      X : Ada.Exceptions.EXCEPTION_OCCURRENCE;
      S : STRING);

   procedure Stop;

   protected Pass is
      procedure Handle (
         Cause: Ada.Task_Termination.CAUSE_OF_TERMINATION;
         T    : Ada.Task_Identification.TASK_ID;
         X    : Ada.Exceptions.EXCEPTION_OCCURRENCE
      );
   end Pass; 

   procedure Set_Handlers;

end Gotcha;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
