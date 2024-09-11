-- control.ads

with Ada.Task_Identification;
with Signals;

package Control is

   ---------------------------------------------------------------------
   -- Base abstract controller
   ---------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Attach(self: in out BASE_CONTROLLER);
   -- Attach self to the current task. 
   -- Mandatory first call in tasks body.

   procedure Detach(self: in out BASE_CONTROLLER);
   -- Detach self from the current task and transfer control to invoker.
   -- Mandatory last call in tasks body.

   procedure Resume(self, target: in out BASE_CONTROLLER);
   -- (A)symmetric coroutines transfer of control.

   procedure Kill(self: in out BASE_CONTROLLER);
   -- Abort the task where self is attached.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Suspend the current task and resume the invoker.

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER;
                    target: access ASYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access target.

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER);
   -- Detach self from the current task and transfer control to target
   -- Mandatory symmetric coroutines last call, except for the last to finish.

   procedure Detach(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   procedure Resume(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access target.

   ---------------------------------------------------------------------
   -- Coroutines
   ---------------------------------------------------------------------

   type BASE_COROUTINE       is not null access all BASE_CONTROLLER'Class;
   type ASYMMETRIC_COROUTINE is not null access all ASYMMETRIC_CONTROLLER'Class;
   type SYMMETRIC_COROUTINE  is not null access all SYMMETRIC_CONTROLLER'Class;

private

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID; -- := Null_Task_Id
         flag    : Signals.SIGNAL;                  -- := FALSE
         invoker : access BASE_CONTROLLER'Class;    -- := NULL
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
