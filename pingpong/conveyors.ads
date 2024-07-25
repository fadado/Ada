-- conveyors.ads

with Ada.Task_Identification;

with Signals;

package Conveyors is

   type CONVEYOR is tagged limited private;

   Conveyor_Error : exception;

   procedure Reset(C: in out CONVEYOR);
   -- (Re)initialize a CONVEYOR to default values

   procedure Suspend(here: in out CONVEYOR);
   -- Wait until a SIGNAL is received here

   procedure Resume(there: in out CONVEYOR);
   -- Resume a task different to the current task

   procedure Resume(here: in out CONVEYOR; there: in out CONVEYOR);
   procedure Resume(here: in out CONVEYOR; there: access CONVEYOR) with Inline;
   -- Resume there, and wait until a SIGNAL is notified here

   procedure Yield(here: in out CONVEYOR; await: BOOLEAN := TRUE);
   -- Suspend the current task after resuming here.back.all

private
   use Ada.Task_Identification;
   use Signals;

   type CONVEYOR is tagged limited
      record
         id   : TASK_ID;        -- defaults to Null_Task_Id
         flag : aliased SIGNAL; -- defaults to false
         back : access  SIGNAL; -- defaults to null
      end record;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
