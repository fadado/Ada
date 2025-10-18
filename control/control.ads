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

   ---------------------------------------------------------------------------
   --  Subsystem common exceptions and types
   ---------------------------------------------------------------------------

   Control_Error : exception;
   --  Generic subsystem exception

   Exit_Controller : exception;
   --  Visible, but to be handled *only* on task body handlers

   Stop_Iteration : exception;
   --  Raised when a controller task has finished

   subtype EXCEPTION_TYPE   is Ada.Exceptions.EXCEPTION_OCCURRENCE;
   subtype EXCEPTION_ACCESS is Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

   Null_Exception : EXCEPTION_TYPE renames Ada.Exceptions.Null_Occurrence;
   --  Simple renaming to simplify naming and avoid `use`

   type VOID is null record;
   --  Common type for child packages and clients using void contexts

   ---------------------------------------------------------------------------
   --  DISPATCHER_TYPE
   ---------------------------------------------------------------------------

   type DISPATCHER_TYPE       is tagged limited private;
   type DISPATCHER_ACCESS     is access all DISPATCHER_TYPE;
   --  A simple controller to attatch to the current task

   procedure Close
     (dispatcher : in out DISPATCHER_TYPE);
   --  Force the exit for a suspended `dispatcher`

   ---------------------------------------------------------------------------
   --  CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   type CONTROLLER_TYPE       is abstract limited new DISPATCHER_TYPE with private;
   type CONTROLLER_ACCESS     is access all CONTROLLER_TYPE;
   --  Base controler

   procedure Commence
     (controller : in out CONTROLLER_TYPE);
   --  Commence `controller` in the current task

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : in EXCEPTION_TYPE := Null_Exception);
   --  Quit `controller` and migrate exceptions to a suspended invoker if
   --  necessary

   procedure Resume
     (controller : in out CONTROLLER_TYPE'Class; -- not a primitive op.!
      invoker    : in out DISPATCHER_TYPE);
   --  Resume `controller` using `invoker` as dispatcher

   procedure Yield
     (controller : in out CONTROLLER_TYPE) is abstract;
   --  Suspend `controller` and transfers control to a suspended invoker

   procedure Resume
     (controller : in out CONTROLLER_TYPE;
      invoker    : in out CONTROLLER_TYPE) is abstract;
   --  Resume `controller` using `invoker` as dispatcher

   ---------------------------------------------------------------------------
   --  SEMI_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   type SEMI_CONTROLLER_TYPE     is limited new CONTROLLER_TYPE with private;
   type SEMI_CONTROLLER_ACCESS   is access all SEMI_CONTROLLER_TYPE;

   overriding procedure Yield
     (controller : in out SEMI_CONTROLLER_TYPE);
   --  Suspend `controller` and transfers control to a suspended invoker

   overriding procedure Resume
     (controller : in out SEMI_CONTROLLER_TYPE;
      invoker    : in out SEMI_CONTROLLER_TYPE)
   with Inline;
   --  Resume `controller` using `invoker` as dispatcher

   ---------------------------------------------------------------------------
   --  FULL_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   type FULL_CONTROLLER_TYPE     is limited new CONTROLLER_TYPE with private;
   type FULL_CONTROLLER_ACCESS   is access all FULL_CONTROLLER_TYPE;

   overriding procedure Yield
     (controller : in out FULL_CONTROLLER_TYPE);
   -- Disabled: raises Program_Error

   overriding procedure Resume
     (controller : in out FULL_CONTROLLER_TYPE;
      invoker    : in out FULL_CONTROLLER_TYPE);
   -- TODO...

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
      end record
   with Type_Invariant =>
     (DISPATCHER_TYPE.state /= EXPECTANT or else DISPATCHER_TYPE.id = Null_Task_Id)
   and then
     (DISPATCHER_TYPE.state /= RUNNING   or else DISPATCHER_TYPE.id = Current_Task)
   ;

   type CONTROLLER_TYPE is abstract limited new DISPATCHER_TYPE with
      record
         link    : DISPATCHER_ACCESS;
      end record;

   type SEMI_CONTROLLER_TYPE is limited new CONTROLLER_TYPE with
      record
         null;
      end record;

   type FULL_CONTROLLER_TYPE is limited new CONTROLLER_TYPE with
      record
         null;
      end record;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
