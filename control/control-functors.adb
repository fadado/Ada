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
     (functor : in out FUNCTOR_TYPE;
      map     : not null access function (x: INPUT_TYPE) return OUTPUT_TYPE)
   is
      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(functor);
   begin
      if functor.inaugural then
         functor.inaugural := FALSE;
         functor.output := map(functor.input);
      else
         parent.Yield;
         functor.output := map(functor.input);
      end if;
   end Yield;

   ------------
   -- Resume --
   ------------

   function Resume
     (functor : in out FUNCTOR_TYPE;
      input   : in INPUT_TYPE) return OUTPUT_TYPE
   is
   begin
      if functor.state = DEAD then
         raise Stop_Iteration;
      end if;

      functor.input := input;
      functor.dispatcher.Dispatch(functor);

      if functor.state = DEAD then
         raise Stop_Iteration;
      end if;

      return functor.output;
   end Resume;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (functor : in out FUNCTOR_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (functor.runner'Terminated);

      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(functor);
   begin
      if functor.state /= DEAD then
         parent.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- Functor_Runner --
   ----------------------

   task body Functor_Runner
   is
   begin
      functor.Commence;
      functor.main(functor.all, functor.context);
      functor.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => functor.Quit(X);
   end Functor_Runner;

end Control . Functors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
