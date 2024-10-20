-- control.ads

private with Ada.Task_Identification;
private with Signals;

with Ada.Exceptions;
use  Ada.Exceptions;

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

   procedure Resume(self, target: in out BASE_CONTROLLER);
   -- Transfers control to `target` ("primary" method).

   procedure Cancel(self: in out BASE_CONTROLLER; X: in EXCEPTION_OCCURRENCE);
   -- Helper for exception handlers.

   type STATUS_TYPE is (RUNNING, SUSPENDED, NORMAL, DEAD);
   -- Tag controller status.

   function Status(self: in out BASE_CONTROLLER) return STATUS_TYPE with Inline;
   -- Returns the status of the coroutine (design stoled from Lua):
   --    RUNNING, if the coroutine is running (that is, it is the one that
   --    called status);
   --    SUSPENDED, if the coroutine is suspended in a call to yield, or if it
   --    has not started running yet;
   --    NORMAL, if the coroutine is active but not running (that is, it has
   --    resumed another coroutine); and
   --    DEAD, if the coroutine has finished its body function, or if it has
   --    stopped with an error.

   function Is_Yieldable(self: in out BASE_CONTROLLER) return BOOLEAN with Inline;
   -- Return FALSE if `Environment_Task` is the task for `self`.

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER);
   -- "Before" method for primary resume.

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   -- Transfers control to the invoker.

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;

   overriding
   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER);
   -- "Before" method for primary resume.

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER);
   -- Transfers control to `target` and detach `self` from the current task.
   -- Mandatory symmetric coroutines last call, except for the last to finish.

private
   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         link    : access BASE_CONTROLLER'Class;
         state   : STATUS_TYPE := SUSPENDED;
         flag    : Signals.SIGNAL;
         migrant : EXCEPTION_OCCURRENCE_ACCESS;
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   -- Stack like pass of control

   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;
   -- Graph like pass of control

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
