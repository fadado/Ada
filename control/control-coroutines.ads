------------------------------------------------------------------------------
--  Control . CoRoutines specification (generic)
------------------------------------------------------------------------------

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

package Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;

   type COROUTINE_TYPE;
   type COROUTINE_ACCESS is access all COROUTINE_TYPE;

   type COROUTINE_FUNCTION is
      not null access procedure (routine: not null COROUTINE_ACCESS);
   --  Procedure access type for the main program

   type COROUTINE_TYPE (main: COROUTINE_FUNCTION; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with *only* transfer of control

   procedure Resume(routine: in out COROUTINE_TYPE);
   --  Resume `routine` and raises `Stop_Iteration` when dead

   procedure Yield(routine: in out COROUTINE_TYPE);
   --  Yields control only

   procedure Close(routine: in out COROUTINE_TYPE);
   --  Force `routine` to exit

private
   task type CoRoutine_Runner (routine: not null COROUTINE_ACCESS);

   type COROUTINE_TYPE (main: COROUTINE_FUNCTION; context: CONTEXT_ACCESS) is
      limited new CONTROLLER_TYPE with
      record
         master : CONTROLLER_TYPE;
         runner : CoRoutine_Runner (COROUTINE_TYPE'Unchecked_Access);
      end record;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
