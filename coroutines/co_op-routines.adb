------------------------------------------------------------------------------
--  Simple routines with only control transfer (implementation)
------------------------------------------------------------------------------

with Control; use Control;

package body Co_Op.Routines is
   ----------
   -- Next --
   ----------

   procedure Next(self: in out ROUTINE_TYPE) is
   begin
      self.head.Resume(ASYMMETRIC_CONTROLLER(self));

      -- is self detached?
      if self.Status = DEAD then
         raise Stop_Iteration;
      end if;
   end Next;

   ----------------
   -- Run_Method --
   ----------------

-- task type Run_Method (self: ROUTINE_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.Program(self);
      self.Detach;
   exception
      when X: others => self.Cancel(X); raise;
   end Run_Method;

   ----------
   -- Wrap --
   ----------

-- generic
--    Program : PROGRAM_ACCESS;
--    Context : CONTEXT_ACCESS := NULL;

   package body Wrap is
      r : ROUTINE_TYPE (PROGRAM, CONTEXT);

      procedure Call is
      begin
         r.Next;
      end Call;
   end Wrap;

end Co_Op.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
