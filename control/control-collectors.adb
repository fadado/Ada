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

   overriding function Yield
     (collector : in out COLLECTOR_TYPE) return ELEMENT_TYPE
   is
      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(collector);
   begin
      if collector.inaugural then
         collector.inaugural := FALSE;
         return collector.input;
      else
         parent.Yield;
         return collector.input;
      end if;
   end Yield;

   ------------
   -- Resume --
   ------------

   not overriding procedure Resume
     (collector : in out COLLECTOR_TYPE;
      input     : in ELEMENT_TYPE)
   is
   begin
      if collector.state = DEAD then
         raise Stop_Iteration;
      end if;

      collector.input := input;
      collector.dispatcher.Dispatch(collector);

      if collector.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (collector : in out COLLECTOR_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (collector.runner'Terminated);

      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(collector);
   begin
      if collector.state /= DEAD then
         parent.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- Collector_Runner --
   ----------------------

   task body Collector_Runner
   is
   begin
      collector.Commence;
      collector.main(collector.all, collector.context);
      collector.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => collector.Quit(X);
   end Collector_Runner;

end Control . Collectors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
