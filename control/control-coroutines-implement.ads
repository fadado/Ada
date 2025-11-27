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

 --overriding procedure Yield
 --  (routine : in out COROUTINE_TYPE)
 --with Inline;
 ----  Yields control only

   not overriding procedure Resume
     (routine : in out COROUTINE_TYPE;
      invoker : in out DISPATCHER_TYPE);
   --  Resume `routine` using `invoker` as a dispatcher

   overriding procedure Resume
     (routine : in out COROUTINE_TYPE;
      invoker : in out COROUTINE_TYPE)
   with Inline;
   --  Resume `routine` using `invoker` as a dispatcher

   procedure Close
     (routine : in out COROUTINE_TYPE);
   --  Force `routine` to exit

private
   task type CoRoutine_Runner (routine: not null access COROUTINE_TYPE);

   type COROUTINE_TYPE (
         main    : COROUTINE_PROCEDURE;
         context : access CONTEXT_TYPE
   ) is limited new BASE_CONTROLLER and COROUTINE_INTERFACE with 
      record
         runner  : CoRoutine_Runner (COROUTINE_TYPE'Access);
      end record;

end Control . CoRoutines . Implement;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
