------------------------------------------------------------------------------
--  Control . Collectors implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . Collectors is
   ---------------------------------------------------------------------------
   --  COLLECTOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   function Yield
     (self : in out COLLECTOR_TYPE) return ELEMENT_TYPE
   is
      super : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(self);
   begin
      if self.inaugural then
         self.inaugural := FALSE;
         return self.input;
      else
         super.Yield;
         return self.input;
      end if;
   end Yield;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (self  : in out COLLECTOR_TYPE;
      input : in ELEMENT_TYPE)
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
   end Resume;

   -----------
   -- Close --
   -----------

   procedure Close
     (self : in out COLLECTOR_TYPE)
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
   -- Collector_Runner --
   ----------------------

   task body Collector_Runner
   is
      self : COLLECTOR_TYPE renames encloser.all;
   begin
      self.Commence;
      self.main(self, self.context);
      self.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => self.Quit(X);
   end Collector_Runner;

end Control . Collectors;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
