-- control.ads

with Ada.Task_Identification;

with Signals;

package Control is

   type CONTROLLER is tagged limited private;

   procedure Attach(self: in out CONTROLLER);
   -- Attach self with the current task

   procedure Resume(self: in out CONTROLLER; target: in out CONTROLLER);
   procedure Resume(self: in out CONTROLLER; target: access CONTROLLER) with Inline;
   -- Suspend the current task after resuming target

   procedure Yield(self: in out CONTROLLER);
   -- Suspend the current task after resuming master

   procedure Detach(self: in out CONTROLLER);
   -- Detach self from the current task and resume master

   procedure Detach(self: in out CONTROLLER; target: in out CONTROLLER);
   procedure Detach(self: in out CONTROLLER; target: access CONTROLLER) with Inline;
   -- Detach self from the current task and resume target

private
   use Ada.Task_Identification;
   use Signals;

   type CONTROLLER is tagged limited
      record
         id     : TASK_ID;            -- := Null_Task_Id
         here   : aliased SIGNAL;     -- := FALSE
         master : access CONTROLLER;  -- := NULL
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
