-- control.ads

with Ada.Task_Identification;

with Signals;

package Control is

   ---------------------------------------------------------------------
   -- Base abstract controller
   ---------------------------------------------------------------------
   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Attach(self: in out BASE_CONTROLLER);
   -- Attach self with the current task. 
   -- Mandatory first call in tasks body.

   procedure Detach(self: in out BASE_CONTROLLER);
   -- Detach self from the current task and transfer control to invoker.
   -- Mandatory last call in tasks body.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------
   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER; target: in out ASYMMETRIC_CONTROLLER);
   procedure Resume(self: in out ASYMMETRIC_CONTROLLER; target: access ASYMMETRIC_CONTROLLER) with Inline;
   -- Symmetric coroutines transfer of control.

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Suspend the current task and resume the invoker.

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------
   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Resume(self: in out SYMMETRIC_CONTROLLER; target: in out SYMMETRIC_CONTROLLER);
   procedure Resume(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) with Inline;
   -- Asymmetric coroutines transfer of control.

   procedure Detach(self: in out SYMMETRIC_CONTROLLER; target: in out SYMMETRIC_CONTROLLER);
   procedure Detach(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) with Inline;
   -- Detach self from the current task and transfer control to target
   -- Mandatory symmetric coroutines last call, except for the last to finish.

private
   use Ada.Task_Identification;
   use Signals;

   type BASE_CONTROLLER is tagged limited
      record
         id      : TASK_ID; -- := Null_Task_Id
         flag    : aliased SIGNAL; -- := FALSE
         invoker : access BASE_CONTROLLER'Class; -- := NULL
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
