------------------------------------------------------------------------------
--  Generic Control . Routines (implementation)
------------------------------------------------------------------------------

package body Control . Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods
   ---------------------------------------------------------------------------

-- type ROUTINE_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is ...

   ------------
   -- Resume --
   ------------

   procedure Resume(self: in out ROUTINE_TYPE) is
   begin
      pragma Assert(self.runner'Callable);

      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      -- is self detached?
      if self.state = DEAD then
         raise Stop_Iterator;
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out ROUTINE_TYPE) is
   begin
      pragma Assert(self.runner'Callable);
      self.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out ROUTINE_TYPE) is
   begin
      self.Request_To_Exit;
      pragma Assert(self.runner'Terminated);
   end Close;

   ----------------
   -- Run_Method --
   ----------------

-- task type Run_Method (self: not null ROUTINE_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.main(self);
      self.Detach;
   exception
      when Exit_Controller => self.Die;
      when X: others       => self.Detach(X);
   end Run_Method;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
