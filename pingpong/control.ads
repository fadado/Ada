-- control.ads

with Ada.Task_Identification;

with Signals;

package Control is

   type CONTROLLER is tagged limited private;

   Control_Error : exception;

   procedure Reset(c: in out CONTROLLER);
   -- (Re)initialize a CONTROLLER to default values

   procedure Suspend(here: in out CONTROLLER);
   -- Wait until a SIGNAL is received here

   procedure Resume(there: in out CONTROLLER);
   -- Resume a task different to the current task

   procedure Resume(here: in out CONTROLLER; there: in out CONTROLLER);
   procedure Resume(here: in out CONTROLLER; there: access CONTROLLER) with Inline;
   -- Resume there, and wait until a SIGNAL is notified here

   procedure Yield(here: in out CONTROLLER; await: BOOLEAN := TRUE);
   -- Suspend the current task after resuming here.back.all

private
   use Ada.Task_Identification;
   use Signals;

   type CONTROLLER is tagged limited
      record
         id   : TASK_ID;        -- := Null_Task_Id
         flag : aliased SIGNAL; -- := FALSE
         back : access  SIGNAL; -- := null
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
