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

-- task type Run_Method (self: ROUTINE_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.main(self);
      self.Detach;

   exception
      when Exit_Controller => self.Die;
      when X: others       => self.Detach(X);
   end Run_Method;

   ---------------------------------------------------------------------------
   --  Wrapper for program with optional context
   ---------------------------------------------------------------------------

   ----------
   -- Wrap --
   ----------

-- generic
--    Main    : PROGRAM_ACCESS;
--    Context : CONTEXT_ACCESS := NULL;

   package body Wrap is
      routine : ROUTINE_TYPE (Main, Context);

      procedure Call is
      begin
         routine.Resume;
      exception
      --  Exceptions raised again and propagated to the caller after cleanup
         when Stop_Iterator =>
            pragma Assert(routine.runner'Terminated);
            raise;
         when others =>
            routine.Close; -- Just in case...
            raise;
      end Call;
   end Wrap;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
