-- co_op.ads

with Control;

package Co_Op is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   Stop_Iteration : exception;

   type TOP_CONTEXT is tagged null record;

   None : aliased TOP_CONTEXT;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   
   generic
      type CONTEXT_TYPE is abstract new TOP_CONTEXT with private;
      --
   package Routine_Types is
      subtype CONTEXT is CONTEXT_TYPE;

      type ROUTINE;

      type ROUTINE_ACCESS is not null access all ROUTINE;
      type CONTEXT_ACCESS is not null access all CONTEXT;

      type PROGRAM_ACCESS is not null access procedure (
         self    : ROUTINE_ACCESS;
         context : CONTEXT_ACCESS
      );

      type ROUTINE (
         program : PROGRAM_ACCESS; 
         context : CONTEXT_ACCESS )
      is limited new Control.ASYMMETRIC_CONTROLLER with private;

      procedure Resume(self: in out ROUTINE);
      procedure Yield(self: in out ROUTINE);

   private
      task type RUNNER (self: ROUTINE_ACCESS);

      type ROUTINE (
         program : PROGRAM_ACCESS;
         context : CONTEXT_ACCESS )
      is limited new Control.ASYMMETRIC_CONTROLLER with
         record
            head : Control.ASYMMETRIC_CONTROLLER;
            run  : RUNNER (ROUTINE'Unchecked_Access);
         end record;
   end Routine_Types;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   
end Co_Op;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
