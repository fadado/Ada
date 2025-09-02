------------------------------------------------------------------------------
--  Control specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

pragma Restrictions (
   No_Abort_Statements,
   No_Select_Statements,
   No_Delay,
   No_Requeue_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Local_Protected_Objects,
   No_Dynamic_Priorities
);

        with Ada.Exceptions;
private with Ada.Synchronous_Task_Control;
private with Ada.Task_Identification;

package Control is

   Control_Error : exception;
   --  Generic subsystem exception

   Exit_Controller : exception;
   --  Visible, but to be handled *only* on task body handlers

   Stop_Iteration : exception;
   --  Raised when a controller task has finished

   type VOID is null record;
   --  Common type for child packages and clients using void contexts

   subtype EXCEPTION_TYPE   is Ada.Exceptions.EXCEPTION_OCCURRENCE;
   subtype EXCEPTION_ACCESS is Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;
   --  Simple renaming to simplify naming and avoid `use`

   ---------------------------------------------------------------------------
   --  CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   type CONTROLLER_TYPE is tagged limited private;
   type CONTROLLER_ACCESS is access all CONTROLLER_TYPE;
   --  Asymmetric and symmetric controlers

   procedure Initiate
     (controller : in out CONTROLLER_TYPE);
   --  Initiate `controller` in the current task

   procedure Quit
     (controller : in out CONTROLLER_TYPE);
   --  Quit `controller` returning control to a suspended invoker

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : in EXCEPTION_TYPE);
   --  Quit and migrate exceptions to a suspended controller

   procedure Die
     (controller : in out CONTROLLER_TYPE);
   --  Set `controller` to the DEAD state

   procedure Resume
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE);
   --  Suspend `controller` and transfers control to `target` (for asymmetric
   --  coroutines)

   procedure Transfer
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE;
      suspend    : in BOOLEAN := TRUE);
   --  Suspend `controller` and transfers control to `target` (for symmetric
   --  coroutines); wait to be resumed only if `suspend`

   procedure Yield
     (controller : in out CONTROLLER_TYPE);
   --  Suspend `controller` and transfers control to a suspended controller

   procedure Request_To_Exit
     (target : in out CONTROLLER_TYPE);
   --  Force to exit a suspended `target`

   ---------------------------------------------------------------------------
   --  DISPATCHER_TYPE
   ---------------------------------------------------------------------------

   type DISPATCHER_TYPE is tagged limited private;
   type DISPATCHER_ACCESS is access all DISPATCHER_TYPE;
   --  Base parent for CONTROLLER_TYPE

   procedure Resume
     (dispatcher : in out DISPATCHER_TYPE;
      controller : in out CONTROLLER_TYPE'Class);
   --  Start controller

private
   ---------------------------------------------------------------------------
   --  A "renaming" layer on top of `Ada.Synchronous_Task_Control`
   ---------------------------------------------------------------------------

   package Signal is
      use Ada.Synchronous_Task_Control;

      subtype T is SUSPENSION_OBJECT;
      --  `FALSE` initialized semaphores for *two* tasks only

      procedure Wait(S: in out T) renames Suspend_Until_True;

      procedure Notify(S: in out T) renames Set_True;

      procedure Clear(S: in out T) renames Set_False;

      function  Is_Set(S: in T) return BOOLEAN renames Current_State;

      function  Is_Cleared(S: in T) return BOOLEAN
         is (not Is_Set(S)) with Inline;
   end Signal;

   ---------------------------------------------------------------------------
   --  Full view for DISPATCHER_TYPE and CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   use Ada.Task_Identification;

   type STATUS_TYPE is (EXPECTANT, SUSPENDED, RUNNING, DEAD, DYING)
      with Default_Value => EXPECTANT;

   type DISPATCHER_TYPE is tagged limited
      record
         id      : TASK_ID;
         state   : STATUS_TYPE;
         run     : Signal.T;
         migrant : EXCEPTION_ACCESS;
      end record;

   type CONTROLLER_TYPE is limited new DISPATCHER_TYPE with
      record
         link    : DISPATCHER_ACCESS;
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
