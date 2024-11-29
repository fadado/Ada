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

        with Ada.Exceptions;
private with Ada.Synchronous_Task_Control;
private with Ada.Task_Identification;

package Control is

   Exit_Controller : exception;
   --  Visible, but raised *only* on `Request_To_Exit` and to be handled
   --  *only* on task body handlers

   Stop_Iteration : exception;
   --  Raised in child packages to indicate iterator exhaustion

   subtype EXCEPTION_TYPE   is Ada.Exceptions.EXCEPTION_OCCURRENCE;
   subtype EXCEPTION_ACCESS is Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;
   --  Simple renaming to avoid `use`

   ---------------------------------------------------------------------------
   --  Base (abstract) controller
   ---------------------------------------------------------------------------

   type BASE_CONTROLLER is abstract tagged limited private;
   --  Common behavior for asymmetric and symmetric controlers

   procedure Attach(controller: in out BASE_CONTROLLER);
   --  Attach `controller` to the current task (raises `Exit_Controller`)

   procedure Detach(controller: in out BASE_CONTROLLER);
   --  Detach `controller` from the current task 

   procedure Detach(controller: in out BASE_CONTROLLER; X: in EXCEPTION_TYPE);
   --  Detach and migrate exceptions to the suspended invoker

   procedure Die(controller: in out BASE_CONTROLLER) with Inline;
   --  Put `controller` to the final state

   procedure Transfer(controller, target: in out BASE_CONTROLLER);
   --  Transfers control to `target` (raises `target` migrated exceptions)

   procedure Request_To_Exit(controller: in out BASE_CONTROLLER);
   --  Force a suspended `controller` to exit

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Stack like transfer of control

   procedure Suspend(controller: in out ASYMMETRIC_CONTROLLER);
   --  Transfers control to the invoker (raises `Exit_Controller`)

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   type SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with private;
   --  Graph like transfer of control

   procedure Jump(controller, target: in out SYMMETRIC_CONTROLLER);
   --  Transfers control to `target` and detach `controller` from the current
   --  task

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

   type STATUS_TYPE is (EXPECTANT, SUSPENDED, RUNNING, DEAD, DYING);

   type BASE_CONTROLLER is abstract tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         state   : STATUS_TYPE := EXPECTANT;
         link    : access BASE_CONTROLLER'Class;
         run     : Signals.SIGNAL;
         migrant : EXCEPTION_ACCESS;
      end record;

   type ASYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;
   type  SYMMETRIC_CONTROLLER is new BASE_CONTROLLER with null record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
