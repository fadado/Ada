-- co_op.adb

with Control;

package body Co_Op is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   package body Routine_Types is
      use Control;

      task body RUNNER is
      begin
         self.Attach;
         self.Program(self, self.context);
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end RUNNER;

      procedure Resume(self: in out ROUTINE) is
         target : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
      begin
         self.head.Resume(target);
         if target.Detached then raise Stop_Iteration; end if;
      end Resume;

      procedure Yield(self: in out ROUTINE) is
         super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
      begin
         super.Yield;
      end Yield;
   end Routine_Types;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
end Co_Op;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
