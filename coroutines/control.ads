------------------------------------------------------------------------------
--  Control specification
------------------------------------------------------------------------------

pragma Restrictions (
   No_Abort_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Select_Statements
);

with Ada.Exceptions; use Ada.Exceptions;

private with Ada.Task_Identification;
private with Ada.Synchronous_Task_Control;

package Control is

   Exit_Controller : exception;
   --  Raised to exit the current task; to handle only on task body

   Stop_Iterator : exception;
   --  Raised to indicate iterator exhaustion

   type VOID is null record;
   --  Helper for void contexts

   ---------------------------------------------------------------------------
   --  Base (abstract) controller
   ---------------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;

   procedure Die(self: in out BASE_CONTROLLER) with Inline;
   --  Put `self` to the final state

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

private
   ---------------------------------------------------------------------------
   --  A "renaming" layer on top of `Ada.Synchronous_Task_Control`
   ---------------------------------------------------------------------------

   package Signals is
      use Ada.Synchronous_Task_Control;

      subtype SIGNAL is SUSPENSION_OBJECT;
      --  `FALSE` initialized semaphores for *two* tasks only

      procedure Wait(S: in out SIGNAL) renames Suspend_Until_True;

      procedure Notify(S: in out SIGNAL) renames Set_True;

      procedure Clear(S: in out SIGNAL) renames Set_False;

      function Is_Set(S: in SIGNAL) return BOOLEAN renames Current_State;

      function Is_Cleared(S: in SIGNAL) return BOOLEAN
         is (not Is_Set(S)) with Inline;
   end Signals;

   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   type STATUS_TYPE is (
      EXPECTANT,  -- not yet born
      SUSPENDED,  -- the controller has not started or called `Suspend`
      RUNNING,    -- the controller is the one that called `Status`
      DEAD,       -- the coroutine has detached or has stopped with an error
      DYING       -- transient state for request to exit
   );

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         state   : STATUS_TYPE := EXPECTANT;
         link    : access BASE_CONTROLLER'Class;
         run     : Signals.SIGNAL;
         migrant : EXCEPTION_OCCURRENCE_ACCESS;
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   type  SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
