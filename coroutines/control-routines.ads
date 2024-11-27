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

   type GENERATOR_FUNCTION is
      not null access procedure (self: not null ROUTINE_ACCESS);
   --  Procedure access type for the main program

   type ROUTINE_TYPE (main: GENERATOR_FUNCTION; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with *only* transfer of control

   procedure Resume(self: in out ROUTINE_TYPE);
   --  Resume `self` and raises `Stop_Iterator` when dead

   procedure Yield(self: in out ROUTINE_TYPE);
   --  Yields control only

   procedure Close(self: in out ROUTINE_TYPE);
   --  Force `self` to exit

private
   task type Run_Method (self: not null ROUTINE_ACCESS);

   type ROUTINE_TYPE (main: GENERATOR_FUNCTION; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER with
      record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Run_Method (ROUTINE_TYPE'Unchecked_Access);
      end record;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
