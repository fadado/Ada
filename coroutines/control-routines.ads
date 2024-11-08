------------------------------------------------------------------------------
--  Generic Control . Routines (specification)
------------------------------------------------------------------------------

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

package Control . Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;
   --  Optional data for the program

   type ROUTINE_TYPE;
   type ROUTINE_ACCESS is not null access all ROUTINE_TYPE;
   --  Forward declarations

   type PROGRAM_ACCESS is not null access procedure (self: ROUTINE_ACCESS);
   --  Procedure type for the main program

   type ROUTINE_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with only transfer of control

   procedure Resume(self: in out ROUTINE_TYPE);
   --  Resume `self` and raises `Stop_Iterator` when dead

   procedure Yield(self: in out ROUTINE_TYPE) with Inline;
   --  Yields control only

   procedure Close(self: in out ROUTINE_TYPE);
   --  Force `self` to exit

   ---------------------------------------------------------------------------
   --  Wrapper for program with optional context
   ---------------------------------------------------------------------------

   generic
      Main    : PROGRAM_ACCESS;
      Context : CONTEXT_ACCESS := NULL;
   package Wrap is
      procedure Call with Inline;
      --  Resume `main`; propagate exceptions after cleanup
   end Wrap;

private
   task type Run_Method (self: ROUTINE_ACCESS);
   --  Call `self.main(self)`; propagate exceptions after cleanup

   type ROUTINE_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER with
      record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Run_Method (ROUTINE_TYPE'Unchecked_Access);
      end record;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
