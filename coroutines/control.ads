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

   procedure Detach(self: in out BASE_CONTROLLER);
   -- Mandatory last call in each task body.

   function Attached(self: in out BASE_CONTROLLER) return BOOLEAN with Inline;
   function Detached(self: in out BASE_CONTROLLER) return BOOLEAN with Inline;
   -- Is the controller still active (or not)?

   procedure Resume(self, target: in out BASE_CONTROLLER);
   -- Transfers control to `target` ("primary" method).

   procedure Resume(target: in out BASE_CONTROLLER) is abstract;
   -- Starter.

   procedure Cancel(self: in out BASE_CONTROLLER; X: in Ada.Exceptions.EXCEPTION_OCCURRENCE);
   -- Helper for exception handlers.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER);
   -- "Before" method for primary resume.

   overriding
   procedure Resume(target: in out ASYMMETRIC_CONTROLLER) with Inline;
   -- Starter.

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to the invoker.

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER);
   -- "Before" method for primary resume.

   overriding
   procedure Resume(target: in out SYMMETRIC_CONTROLLER) with Inline;
   -- Starter.

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to `target` and detach `self` from the current task.
   -- Mandatory symmetric coroutines last call, except for the last to finish.

   ---------------------------------------------------------------------
   -- Generic references to controllers
   ---------------------------------------------------------------------

   type ASYMMETRIC_COROUTINE is not null access all ASYMMETRIC_CONTROLLER'Class;
   type SYMMETRIC_COROUTINE  is not null access all SYMMETRIC_CONTROLLER'Class;

private
   ---------------------------------------------------------------------
   -- Controller implementation
   ---------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         flag    : Signals.SIGNAL;
         migrant : Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;
         link    : access BASE_CONTROLLER'Class;
      end record;
   -- := (Null_Task_Id, FALSE, NULL, NULL);

   -- Stack like pass of control
   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;

   -- Graph like pass of control
   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
