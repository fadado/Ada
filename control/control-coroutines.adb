------------------------------------------------------------------------------
--  Control . CoRoutines implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   overriding procedure Yield
     (routine : in out COROUTINE_TYPE)
   is
      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(routine);
   begin
      parent.Yield;
   end Yield;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (routine : in out COROUTINE_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (routine.runner'Terminated);

      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(routine);
   begin
      if routine.state /= DEAD then
         parent.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ------------
   -- Resume --
   ------------

   overriding procedure Resume
     (routine : in out COROUTINE_TYPE;
      invoker : in out COROUTINE_TYPE)
   is
   begin
      CoRoutines.Resume(routine, DISPATCHER_TYPE(invoker));
   end Resume;

   ------------
   -- Resume --
   ------------

   not overriding procedure Resume
     (routine    : in out COROUTINE_TYPE;
      invoker    : in out DISPATCHER_TYPE)
   is
   begin
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      Control.Resume(routine, invoker);

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   ----------------------
   -- CoRoutine_Runner --
   ----------------------

   task body CoRoutine_Runner
   is
   begin
      routine.Commence;
      routine.main(routine.all, routine.context);
      routine.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => routine.Quit(X);
   end CoRoutine_Runner;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
