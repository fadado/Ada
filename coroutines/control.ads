-- control.ads

private with Ada.Task_Identification;
private with Signals;

package Control is
   ---------------------------------------------------------------------
   -- Base abstract controller
   ---------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Attach(self: in out BASE_CONTROLLER);
   -- Attach `self` to the current task. 
   -- Mandatory first call in each task body.

   procedure Detach(self: in out BASE_CONTROLLER);
   -- Transfers control to the invoker.
   -- Mandatory last call in each task body.

   procedure Resume(self, target: in out BASE_CONTROLLER);
   -- Transfers control to `target`.

   procedure Cancel(self: in out BASE_CONTROLLER);
   -- Helper for exception handlers.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to the invoker.

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER;
                    target: access ASYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access `target`.

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to `target` and detach `self` from the current task.
   -- Mandatory symmetric coroutines last call, except for the last to finish.

   procedure Detach(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   procedure Resume(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access `target`.

private
   ---------------------------------------------------------------------
   -- Controllers implementation
   ---------------------------------------------------------------------

   use Ada.Task_Identification;
   use Signals;

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : TASK_ID;                      -- := Null_Task_Id
         flag    : SIGNAL;                       -- := FALSE
         invoker : access BASE_CONTROLLER'Class; -- := NULL
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
