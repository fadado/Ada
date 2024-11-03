------------------------------------------------------------------------------
--  Simple routines with only transfer of control (implementation)
------------------------------------------------------------------------------

package body Control.Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods
   ---------------------------------------------------------------------------

-- type ROUTINE_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is ...

   ------------
   -- Resume --
   ------------

   procedure Resume(self: in out ROUTINE_TYPE) is
   begin
      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      -- is self detached?
      if self.state = Control.DEAD then
         raise Co_Op.Stop_Iterator;
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out ROUTINE_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      super.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out ROUTINE_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      super.Request_To_Exit;
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
   --  All exceptions are raised again for Ada.Task_Termination handling
      when Exit_Controller =>
         if self.state /= Control.DEAD then
            self.Reset;
         end if;
         pragma Assert(self.state = Control.DEAD);
         raise;
      when X: others =>
         self.Detach(X); -- migrates exception to back controller
         pragma Assert(self.state = Control.DEAD);
         raise;
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
      --  All exceptions are raised again and propagated to the caller
         when Co_Op.Stop_Iterator =>
            pragma Assert(routine.state = Control.DEAD);
            raise;
         when others =>
            routine.Close; -- ensure `routine` body has exited
            pragma Assert(routine.state = Control.DEAD);
            raise;
      end Call;
   end Wrap;

end Control.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
