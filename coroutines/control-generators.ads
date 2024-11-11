------------------------------------------------------------------------------
--  Generic Control . Generators (specification)
------------------------------------------------------------------------------

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

   type DATUM_TYPE is private;
   --  Type for `Yield` generated values

package Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;
   --  Optional data for the program

   type GENERATOR_TYPE;
   type GENERATOR_ACCESS is not null access all GENERATOR_TYPE;
   --  Forward declarations

   type PROGRAM_ACCESS is not null access procedure (self: GENERATOR_ACCESS);
   --  Procedure type for the main program

   type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with only transfer of control

   procedure Next(self: in out GENERATOR_TYPE; datum: out DATUM_TYPE);
   --  Resume `self` and raises `Stop_Iterator` when dead

   procedure Yield(self: in out GENERATOR_TYPE; datum: DATUM_TYPE);
   --  Yields control only

   procedure Close(self: in out GENERATOR_TYPE);
   --  Force `self` to exit

   ---------------------------------------------------------------------------
   --  Wrapper for program with optional context
   ---------------------------------------------------------------------------

   generic
      Main    : PROGRAM_ACCESS;
      Context : CONTEXT_ACCESS := NULL;
   package Wrap is
      procedure Call(datum: out DATUM_TYPE);
      --  Resume `main`; propagate exceptions after cleanup
   end Wrap;

private
   task type Run_Method (self: GENERATOR_ACCESS);
   --  Call `self.main(self)`; propagate exceptions after cleanup

   type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER with
      record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Run_Method (GENERATOR_TYPE'Unchecked_Access);
         datum  : DATUM_TYPE;
      end record;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
