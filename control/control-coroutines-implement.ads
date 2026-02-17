------------------------------------------------------------------------------
--  Control . CoRoutines . Implement specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

generic
   type BASE_CONTROLLER is new CONTROLLER_TYPE with private;
   --  Semi or full controller to derive from

   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the coroutine procedure

package Control . CoRoutines . Implement is
   pragma Elaborate_Body(Implement);

   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type COROUTINE_PROCEDURE is not null access procedure
     (routine : in out COROUTINE_INTERFACE'Class;
      context : access CONTEXT_TYPE);
   --  Procedure type for the coroutine procedure

   type COROUTINE_TYPE (
      main    : COROUTINE_PROCEDURE;
      context : access CONTEXT_TYPE
   ) is limited new COROUTINE_INTERFACE with private;
   --  Coroutine type with *only* transfer of control

   procedure Resume
     (self       : in out COROUTINE_TYPE;
      dispatcher : in out DISPATCHER_TYPE);
   --  Resume `self` using `dispatcher`

   procedure Close
     (self : in out COROUTINE_TYPE);
   --  Force `self` to exit

private

   overriding procedure Resume
     (self    : in out COROUTINE_TYPE;
      invoker : in out COROUTINE_TYPE)
   with Inline;
   --  Resume `self` using `invoker` as a dispatcher

   -- Note: Yield is inherited from BASE_CONTROLLER

   task type CoRoutine_Runner (encloser: not null access COROUTINE_TYPE);

   type COROUTINE_TYPE (
         main    : COROUTINE_PROCEDURE;
         context : access CONTEXT_TYPE
   ) is limited new BASE_CONTROLLER and COROUTINE_INTERFACE with
      record
         runner  : CoRoutine_Runner (COROUTINE_TYPE'Access);
      end record;

end Control . CoRoutines . Implement;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
