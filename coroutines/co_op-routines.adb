---------------------------------------------------------------------------
--  Implementation of simple routines with only control transfer
---------------------------------------------------------------------------

with Control; use Control;

package body Co_Op.Routines is

   ------------
   -- RUNNER --
   ------------

   task body RUNNER is
   begin
      self.Attach;
      self.Program(self, self.context);
      self.Detach;
   exception
      when X: others => self.Cancel(X); raise;
   end RUNNER;

   ------------
   -- Resume --
   ------------

   procedure Resume(self: in out ROUTINE; context: CONTEXT_ACCESS) is
   begin
      pragma Assert(self.context = NULL);
      self.context := context;

      self.head.Resume(ASYMMETRIC_CONTROLLER(self));

      if self.Status = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   procedure Resume(self: in out ROUTINE) is
   begin
      self.head.Resume(ASYMMETRIC_CONTROLLER(self));

      if self.Status = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out ROUTINE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      super.Yield;
   end Yield;

   ----------
   -- Wrap --
   ----------

   package body Wrap is
      r : ROUTINE (Thunk); -- Thunk is a generic parameter

      procedure Call is
      begin
         r.Resume;
      end Call;
   end Wrap;

end Co_Op.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
