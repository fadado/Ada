-- control.ads

with Ada.Task_Identification;

with Signals;

package Control is

   type CONTROLLER is tagged limited private;

   procedure Attach(self: in out CONTROLLER);
   -- Attach self with the current task. 
   -- Mandatory tasks first call.

   procedure Detach(self: in out CONTROLLER) with Inline;
   -- Detach self from the current task and transfer control to invoker.
   -- Mandatory asymmetric coroutines last call.

   procedure Resume(self: in out CONTROLLER; target: in out CONTROLLER);
   -- Symmetric coroutines transfer of control.

   procedure Yield(self: in out CONTROLLER);
   -- Suspend the current task after resuming the invoker.

   procedure Transfer(self: in out CONTROLLER; target: in out CONTROLLER);
   -- Asymmetric coroutines transfer of control.

   procedure Detach(self: in out CONTROLLER; target: in out CONTROLLER);
   -- Detach self from the current task and transfer control to target
   -- Mandatory symmetric coroutines last call, except for the last to finish.

   -- Syntactic sugar
   procedure Resume(self: in out CONTROLLER; target: access CONTROLLER) with Inline;
   procedure Detach(self: in out CONTROLLER; target: access CONTROLLER) with Inline;
   procedure Transfer(self: in out CONTROLLER; target: access CONTROLLER) with Inline;

private
   use Ada.Task_Identification;
   use Signals;

   type CO_MODE is (UNDEFINED, ASYMMETRIC, SYMMETRIC);

   type CONTROLLER is tagged limited
      record
         id      : TASK_ID;            -- := Null_Task_Id
         flag    : aliased SIGNAL;     -- := FALSE
         invoker : access CONTROLLER;  -- := NULL
         mode    : CO_MODE := UNDEFINED;
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
