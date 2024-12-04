------------------------------------------------------------------------------
--  Control . Routines specification (generic)
------------------------------------------------------------------------------

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

package Control . Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;

   type ROUTINE_TYPE;
   type ROUTINE_ACCESS is access all ROUTINE_TYPE;

   type ROUTINE_FUNCTION is
      not null access procedure (routine: not null ROUTINE_ACCESS);
   --  Procedure access type for the main program

   type ROUTINE_TYPE (main: ROUTINE_FUNCTION; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with *only* transfer of control

   procedure Resume(routine: in out ROUTINE_TYPE);
   --  Resume `routine` and raises `Stop_Iteration` when dead

   procedure Yield(routine: in out ROUTINE_TYPE);
   --  Yields control only

   procedure Close(routine: in out ROUTINE_TYPE);
   --  Force `routine` to exit

private
   task type Routine_Runner (routine: not null ROUTINE_ACCESS);

   type ROUTINE_TYPE (main: ROUTINE_FUNCTION; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER with
      record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Routine_Runner (ROUTINE_TYPE'Unchecked_Access);
      end record;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
