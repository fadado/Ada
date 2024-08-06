-- control.ads

with Ada.Task_Identification;

with Signals;

package Control is

   type CONTROLLER is tagged limited private;

   procedure Co_Begin(self: in out CONTROLLER);
   -- Wait until a SIGNAL is notified

   procedure Co_End(self: in out CONTROLLER);
   -- Finish the current task resuming back

   procedure Resume(self: in out CONTROLLER; co: in out CONTROLLER);
   procedure Resume(self: in out CONTROLLER; co: access CONTROLLER) with Inline;
   -- Resume co, and wait until a SIGNAL is notified

   procedure Yield(self: in out CONTROLLER);
   -- Suspend the current task after resuming back

   procedure Jump(self: in out CONTROLLER; co: in out CONTROLLER);
   procedure Jump(self: in out CONTROLLER; co: access CONTROLLER) with Inline;
   -- Resume co, but do not wait; reset self to default values

private
   use Ada.Task_Identification;
   use Signals;

   type CONTROLLER is tagged limited
      record
         id   : TASK_ID;            -- := Null_Task_Id
         here : aliased SIGNAL;     -- := FALSE
         back : access CONTROLLER;  -- := NULL
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
