------------------------------------------------------------------------------
--  Simple routines with only control transfer (specification)
------------------------------------------------------------------------------

with Control;

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

package Co_Op.Routines is

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;
   --  Optional data for the program

   type ROUTINE_TYPE;
   type ROUTINE_ACCESS is not null access all ROUTINE_TYPE;
   --  Forward declarations

   type PROGRAM_ACCESS is not null access procedure (self: ROUTINE_ACCESS);
   --  Procedure type for the program to run

   type ROUTINE_TYPE (program: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      limited new Control.ASYMMETRIC_CONTROLLER with private;
   --  Coroutine type with only transfer of control

   procedure Next(self: in out ROUTINE_TYPE);
   --  Resume `self` and raises `Stop_Iteration` when dead

   generic
      Program : PROGRAM_ACCESS;
      Context : CONTEXT_ACCESS := NULL;
   package Wrap is
      procedure Call with Inline;
   end Wrap;
   --  Wrapper for program references with optional context

private
   task type Run_Method (self: ROUTINE_ACCESS);

   type ROUTINE_TYPE (program: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      limited new Control.ASYMMETRIC_CONTROLLER with
      record
         head    : Control.ASYMMETRIC_CONTROLLER;
         run     : Run_Method (ROUTINE_TYPE'Unchecked_Access);
      end record;

end Co_Op.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
