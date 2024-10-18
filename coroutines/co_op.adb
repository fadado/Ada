-- co_op.adb

with Control;

package body Co_Op is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   package body Routine_Types is

      ------------
      -- RUNNER --
      ------------

      task body RUNNER is
      begin
         self.Attach;
         -- here after first resume
         self.Program(self, self.context);
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end RUNNER;

      ------------
      -- Resume --
      ------------

      procedure Resume(self: in out ROUTINE; context: CONTEXT_ACCESS) is
         use type Control.STATUS_TYPE;
      begin
         pragma Assert(self.context = NULL);
         self.context := context;
         self.head.Resume(Control.ASYMMETRIC_CONTROLLER(self));
         if self.Status = Control.DEAD then
            raise Stop_Iteration;
         end if;
      end Resume;

      ------------
      -- Resume --
      ------------

      procedure Resume(self: in out ROUTINE) is
         use type Control.STATUS_TYPE;
      begin
         self.head.Resume(Control.ASYMMETRIC_CONTROLLER(self));
         if self.Status = Control.DEAD then
            raise Stop_Iteration;
         end if;
      end Resume;

      -----------
      -- Yield --
      -----------

      procedure Yield(self: in out ROUTINE) is
      begin
         -- delegate to super
         Control.ASYMMETRIC_CONTROLLER(self).Yield;
      end Yield;
   end Routine_Types;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
end Co_Op;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
