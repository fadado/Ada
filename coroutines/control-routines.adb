------------------------------------------------------------------------------
--  Simple routines with only transfer of control (implementation)
------------------------------------------------------------------------------

package body Control.Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods
   ---------------------------------------------------------------------------

-- type ROUTINE_TYPE (Main: PROGRAM_ACCESS; Context: CONTEXT_ACCESS) is ...

   ------------
   -- Resume --
   ------------

   procedure Resume(self: in out ROUTINE_TYPE) is
   begin
      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      -- is self detached?
      if self.Status = Control.DEAD then
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
      super.Stop;
   end Close;

   ----------------
   -- Run_Method --
   ----------------

-- task type Run_Method (self: ROUTINE_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.Main(self);
      self.Detach;
   exception
      when Exit_Controller => null;
      when X: others => self.Migrate(X);
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
         when Co_Op.Stop_Iterator => raise;
         when others => routine.Close; raise;
      end Call;
   end Wrap;

end Control.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
