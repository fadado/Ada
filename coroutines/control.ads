------------------------------------------------------------------------------
--  Transfer of control specification
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

private with Ada.Task_Identification;
private with Signals;

package Control is
   ---------------------------------------------------------------------------
   --  Base abstract controller
   ---------------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Attach(self: in out BASE_CONTROLLER);
   --  Attach `self` to the current task 

   procedure Detach(self: in out BASE_CONTROLLER);
   --  Detach `self` from the current task 

   procedure Resume(self, target: in out BASE_CONTROLLER);
   --  Transfer control to `target` ("primary" method)

   procedure Cancel(self: in out BASE_CONTROLLER; X: in EXCEPTION_OCCURRENCE);
   --  Helper for exception handlers

   type STATUS_TYPE is (
      SUSPENDED,  --  the controller has not started running or called `Yield`
      RUNNING,    --  the controller is the one that called `Status`
      NORMAL,     --  the coroutine is active but not running (that is, it
                  --  has resumed another controller)
      DEAD        --  the coroutine has finished its body function, or it
                  --  has stopped with an error
   );

   function Status(self: in out BASE_CONTROLLER) return STATUS_TYPE
     with Inline;
   --  Return the controller status

   function Is_Yieldable(self: in out BASE_CONTROLLER) return BOOLEAN
     with Inline;
   --  Return `TRUE` if `Environment_Task` is *not* the task for `self`.

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Stack like transfer of control

   overriding
   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER);
   --  "Before" method for primary `Resume`

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER);
   --  Transfer control to the invoker

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Graph like transfer of control

   overriding
   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER);
   --  "Before" method for primary `Resume`

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER);
   --  Transfers control to `target` and detach `self` from the current task

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

   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
