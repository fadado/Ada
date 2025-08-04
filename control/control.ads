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

   type VOID is null record;
   --  Facility for child packages and clients

   ---------------------------------------------------------------------------
   --  CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   type CONTROLLER_TYPE is tagged limited private;
   type CONTROLLER_ACCESS is access all CONTROLLER_TYPE;
   --  Common behavior for asymmetric and symmetric controlers

   procedure Initiate(controller: in out CONTROLLER_TYPE);
   --  Initiate `controller` to the current task (raises `Exit_Controller`)

   procedure Suspend(controller: in out CONTROLLER_TYPE);
   --  Transfers control to the invoker (raises `Exit_Controller`)

   procedure Quit(controller: in out CONTROLLER_TYPE);
   --  Quit `controller` from the current task 

   procedure Quit(controller: in out CONTROLLER_TYPE; X: in EXCEPTION_TYPE);
   --  Quit and migrate exceptions to the suspended invoker

   procedure Reset(controller: in out CONTROLLER_TYPE) with Inline;
   --  Put `controller` to the final state

   procedure Call(controller, target: in out CONTROLLER_TYPE);
   --  TODO

   procedure Transfer(controller, target: in out CONTROLLER_TYPE);
   --  Transfers control to `target` (raises `target` migrated exceptions)

   procedure Jump(controller, target: in out CONTROLLER_TYPE);
   --  Transfers control to `target`

   procedure Request_To_Exit(target: in out CONTROLLER_TYPE);
   --  Force a suspended `target` to exit

private
   ---------------------------------------------------------------------------
   --  A "renaming" layer on top of `Ada.Synchronous_Task_Control`
   ---------------------------------------------------------------------------

   package Signal is
      use Ada.Synchronous_Task_Control;

      subtype SIGNAL is SUSPENSION_OBJECT;
      --  `FALSE` initialized semaphores for *two* tasks only

      procedure Wait(S: in out SIGNAL) renames Suspend_Until_True;

      procedure Notify(S: in out SIGNAL) renames Set_True;

      procedure Clear(S: in out SIGNAL) renames Set_False;

      function  Is_Set(S: in SIGNAL) return BOOLEAN renames Current_State;

      function  Is_Cleared(S: in SIGNAL) return BOOLEAN
         is (not Is_Set(S)) with Inline;
   end Signal;

   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   type STATUS_TYPE is (EXPECTANT, SUSPENDED, RUNNING, DEAD, DYING);

   type CONTROLLER_TYPE is tagged limited
      record
         id      : Ada.Task_Identification.TASK_ID;
         state   : STATUS_TYPE := EXPECTANT;
         link    : CONTROLLER_ACCESS;
         run     : Signal.SIGNAL;
         migrant : EXCEPTION_ACCESS;
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
