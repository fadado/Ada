---------------------------------------------------------------------------
--  Specification of simple routines with only control transfer
---------------------------------------------------------------------------

with Control;

generic
   type CONTEXT_TYPE is private; -- in Ada 2022: "or use NONE"
package Co_Op.Routines is

   ------------------------------------------------------------------------
   --  Exported types
   ------------------------------------------------------------------------

   type ROUTINE;
   type ROUTINE_ACCESS is not null access all ROUTINE;
   type CONTEXT_ACCESS is          access all CONTEXT_TYPE;

   type PROGRAM_ACCESS is not null access procedure (
      self    : ROUTINE_ACCESS;
      context : CONTEXT_ACCESS
   );

   ------------------------------------------------------------------------
   --  Basic asymmetric (co)routine transfer of control
   ------------------------------------------------------------------------

   type ROUTINE (program : PROGRAM_ACCESS) is
      limited new Control.ASYMMETRIC_CONTROLLER with private;

   not overriding
   procedure Resume(self: in out ROUTINE; context: CONTEXT_ACCESS);

   not overriding
   procedure Resume(self: in out ROUTINE);

   overriding
   procedure Yield(self: in out ROUTINE);

   ------------------------------------------------------------------------
   --  Wrapper for program references
   ------------------------------------------------------------------------

   generic
      Thunk : PROGRAM_ACCESS;
   package Wrap is
      procedure Call with Inline;
   end Wrap;

private
   task type RUNNER (self: ROUTINE_ACCESS);

   type ROUTINE (program : PROGRAM_ACCESS) is
      limited new Control.ASYMMETRIC_CONTROLLER with
      record
         head    : Control.ASYMMETRIC_CONTROLLER;
         context : CONTEXT_ACCESS := NULL;
         run     : RUNNER (ROUTINE'Unchecked_Access);
      end record;

end Co_Op.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
