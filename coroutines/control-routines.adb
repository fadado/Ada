------------------------------------------------------------------------------
--  Simple routines with only transfer of control (implementation)
------------------------------------------------------------------------------

package body Control.Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods
   ---------------------------------------------------------------------------

-- type ROUTINE_TYPE (Main: PROGRAM_ACCESS; Context: CONTEXT_ACCESS) is ...

   ----------
   -- Next --
   ----------

   procedure Next(self: in out ROUTINE_TYPE) is
      use Co_Op;
   begin
      self.head.Resume(ASYMMETRIC_CONTROLLER(self));

      -- is self detached?
      if self.Status = DEAD then
         raise Stop_Iterator;
      end if;
   end Next;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out ROUTINE_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      super.Yield;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out ROUTINE_TYPE) is
   begin
      self.Request_To_Exit;
      loop
         exit when self.Status = DEAD;
      end loop;
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
         use Co_Op;
      begin
         routine.Next;
      exception
         when Stop_Iterator =>
            raise;
         when others =>
            if routine.Status /= DEAD then
               routine.Close;
            end if;
            raise;
      end Call;
   end Wrap;

end Control.Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
