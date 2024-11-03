------------------------------------------------------------------------------
--  Control specification
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

private with Ada.Task_Identification;
private with Signals;

package Control is

   Exit_Controller : exception;
   --  Raised to exit the current task; to handle only on task body

   ---------------------------------------------------------------------------
   --  Base (abstract) controller
   ---------------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Reset(self: in out BASE_CONTROLLER) with Inline;
   --  Restore `self` to the initial state

   type STATUS_TYPE is (
      SUSPENDED,  --  the controller has not started or called `Suspend`
      RUNNING,    --  the controller is the one that called `Status`
      DEAD,       --  the coroutine has detached or has stopped with an error
      DYING       --  transient state for request to exit
   );

   function Status(self: in BASE_CONTROLLER) return STATUS_TYPE
     with Inline;
   --  Return the controller status

   procedure Attach(self: in out BASE_CONTROLLER);
   --  Attach `self` to the current task (raises Exit_Controller)

   procedure Detach(self: in out BASE_CONTROLLER);
   --  Detach `self` from the current task 

   procedure Detach(self: in out BASE_CONTROLLER; X: in EXCEPTION_OCCURRENCE);
   --  Detach and propagate exception to invoker

   procedure Transfer(self, target: in out BASE_CONTROLLER);
   --  Transfer control to `target` (raises `target` migrated exceptions)

   procedure Request_To_Exit(self: in out BASE_CONTROLLER);
   --  Force `self`, that must be dead or suspended, to exit

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Stack like transfer of control

   procedure Suspend(self: in out ASYMMETRIC_CONTROLLER);
   --  Transfer control to the invoker (raises Exit_Controller)

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Graph like transfer of control

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER);
   --  Transfers control to `target` and detach `self` from the current task

   ---------------------------------------------------------------------------
   --  Common facilities for cooperative routines
   ---------------------------------------------------------------------------

   --  Used in `Control` child units

   package Co_Op is

      type NONE is null record;
      --  Helper for void contexts

      Stop_Iterator : exception;
      --  Raised to indicate iterator exhaustion

   end Co_Op;

private
   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         state   : STATUS_TYPE := SUSPENDED;
         link    : access BASE_CONTROLLER'Class;
         flag    : Signals.SIGNAL;
         migrant : EXCEPTION_OCCURRENCE_ACCESS;
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;

   type SYMMETRIC_CONTROLLER  is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
