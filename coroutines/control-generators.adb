------------------------------------------------------------------------------
--  Generic Control . Generators (implementation)
------------------------------------------------------------------------------

package body Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE methods
   ---------------------------------------------------------------------------

-- type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is ...

   ----------
   -- Next --
   ----------

   procedure Next(self: in out GENERATOR_TYPE; datum: out DATUM_TYPE) is
   begin
      pragma Assert(self.runner'Callable);

      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      if self.state = DEAD then
         raise Co_Op.Stop_Iterator;
      end if;

      datum := self.datum;
   end Next;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out GENERATOR_TYPE; datum: DATUM_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      pragma Assert(self.runner'Callable);

      self.datum := datum;
      super.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out GENERATOR_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      super.Request_To_Exit;
      pragma Assert(self.state = DEAD);
      --TODO: spin_until???
      loop exit when self.runner'Terminated; end loop;
   end Close;

   ----------------
   -- Run_Method --
   ----------------

-- task type Run_Method (self: GENERATOR_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.main(self);
      self.Detach;

   exception
      when Exit_Controller => self.Die;
      when X: others => self.Detach(X);
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
      generator : GENERATOR_TYPE (Main, Context);

      procedure Call(datum: out DATUM_TYPE) is
      begin
         generator.Next(datum);

      exception
      --  Exceptions raised again and propagated to the caller after cleanup
         when Co_Op.Stop_Iterator =>
            pragma Assert(generator.runner'Terminated);
            raise;
         when others =>
            generator.Close; -- Just in case...
            raise;
      end Call;
   end Wrap;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
