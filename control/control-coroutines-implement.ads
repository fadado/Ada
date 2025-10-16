------------------------------------------------------------------------------
--  Control . CoRoutines . Implement specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

generic
   type BASE_CONTROLLER is new CONTROLLER_TYPE with private;
   --  Semi or full controller to derive from

   type CONTEXT_TYPE (<>) is private;
   --  Data to provide an environment for the coroutine procedure

package Control . CoRoutines . Implement is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;

   type COROUTINE_PROCEDURE is not null access procedure
      (routine : in out COROUTINE_INTERFACE'Class;
       context : in CONTEXT_ACCESS);
   --  Procedure type for the coroutine procedure

   type COROUTINE_TYPE (
      main    : COROUTINE_PROCEDURE;
      context : CONTEXT_ACCESS
   ) is limited new COROUTINE_INTERFACE with private;
   --  Coroutine type with *only* transfer of control

   type COROUTINE_ACCESS is access all COROUTINE_TYPE;

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

   overriding procedure Close
     (routine : in out COROUTINE_TYPE);
   --  Force `routine` to exit

private
   task type CoRoutine_Runner (routine: not null COROUTINE_ACCESS);

   type COROUTINE_TYPE (
         main    : COROUTINE_PROCEDURE;
         context : CONTEXT_ACCESS
   ) is limited new BASE_CONTROLLER and COROUTINE_INTERFACE with 
      record
         runner  : CoRoutine_Runner (COROUTINE_TYPE'Unchecked_Access);
      end record;

end Control . CoRoutines . Implement;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
