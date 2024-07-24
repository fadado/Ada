-- conveyors.ads

with Ada.Task_Identification;

with Signals;

package Conveyors is

   type CONVEYOR is tagged limited private;

   procedure Reset(self: in out CONVEYOR);
   -- (Re)initialize a CONVEYOR to default values

   procedure Suspend(self: in out CONVEYOR);
   -- Wait until a SIGNAL is notified

   procedure Resume(target: in out CONVEYOR);
   -- Resume a task different to the current task

   procedure Resume(self: in out CONVEYOR; target: in out CONVEYOR);
   -- Resume the target task, and wait until a SIGNAL is notified

   procedure Yield(self: in out CONVEYOR; Await: BOOLEAN := TRUE);
   -- Suspend the current task after resuming the first resumer

   Conveyor_Error : exception;

   procedure Resume(self: in out CONVEYOR; target: access CONVEYOR) with Inline;

private
   use Ada.Task_Identification;
   use Signals;

   type CONVEYOR is tagged limited
      record
         id   : TASK_ID;        -- defaults to Null_Task_Id
         here : aliased SIGNAL; -- defaults to false
         back : access  SIGNAL; -- defaults to null
      end record;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
