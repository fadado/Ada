------------------------------------------------------------------------------
--  Control . Functors implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . Functors is
   ---------------------------------------------------------------------------
   --  FUNCTOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   procedure Yield
     (self : in out FUNCTOR_TYPE;
      map  : not null access function (x: INPUT_TYPE) return OUTPUT_TYPE)
   is
      super : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(self);
   begin
      if self.inaugural then
         self.inaugural := FALSE;
         self.output := map(self.input);
      else
         super.Yield;
         self.output := map(self.input);
      end if;
   end Yield;

   ------------
   -- Resume --
   ------------

   function Resume
     (self : in out FUNCTOR_TYPE;
      input   : in INPUT_TYPE) return OUTPUT_TYPE
   is
   begin
      if self.state = DEAD then
         raise Stop_Iteration;
      end if;

      self.input := input;
      self.dispatcher.Dispatch(self);

      if self.state = DEAD then
         raise Stop_Iteration;
      end if;

      return self.output;
   end Resume;

   -----------
   -- Close --
   -----------

   procedure Close
     (self : in out FUNCTOR_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (self.runner'Terminated);

      super : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(self);
   begin
      if self.state /= DEAD then
         super.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- Functor_Runner --
   ----------------------

   task body Functor_Runner
   is
      self : FUNCTOR_TYPE renames reference.all;
   begin
      self.Commence;
      self.main(self, self.context);
      self.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => self.Quit(X);
   end Functor_Runner;

end Control . Functors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
