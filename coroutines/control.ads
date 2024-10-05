-- control.ads

with Ada.Exceptions;

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

   procedure Detach(self: in out BASE_CONTROLLER) is abstract;
   -- Mandatory last call in each task body.

   procedure Resume(self, target: in out BASE_CONTROLLER);
   -- Transfers control to `target`.

   procedure Cancel(self: in out BASE_CONTROLLER;
                    X: in Ada.Exceptions.EXCEPTION_OCCURRENCE) is abstract;
   -- Helper for exception handlers.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Detach(self: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to the invoker.

   overriding
   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to `target`.

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to the invoker.

   overriding
   procedure Cancel(self: in out ASYMMETRIC_CONTROLLER;
                    X: in Ada.Exceptions.EXCEPTION_OCCURRENCE);
   -- Helper for exception handlers.

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER;
                    target: access ASYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access `target` (perhaps not inlined!).

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Detach(self: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to the head.

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to `target` and detach `self` from the current task.
   -- Mandatory symmetric coroutines last call, except for the last to finish.

   overriding
   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to `target`.

   overriding
   procedure Cancel(self: in out SYMMETRIC_CONTROLLER;
                    X: in Ada.Exceptions.EXCEPTION_OCCURRENCE);
   -- Helper for exception handlers.

   procedure Detach(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   procedure Resume(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) with Inline;
   -- Syntactic sugar to allow access `target` (perhaps not inlined!).

private
   ---------------------------------------------------------------------
   -- Controller implementation
   ---------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         flag    : Signals.SIGNAL;
         migrant : Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;
      end record;
   -- := (Null_Task_Id, FALSE, NULL);

   -- Stack like pass of control
   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with
      record
         invoker : access ASYMMETRIC_CONTROLLER'Class;
      end record;
   -- := (Null_Task_Id, FALSE, NULL, NULL);

   -- Graph like pass of control
   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with
      record
         head : access SYMMETRIC_CONTROLLER'Class;
      end record;
   -- := (Null_Task_Id, FALSE, NULL, NULL);

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
