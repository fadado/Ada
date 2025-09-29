------------------------------------------------------------------------------
--  Control . CoRoutines implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   not overriding procedure Resume
     (routine    : in out COROUTINE_TYPE;
      invoker    : in out DISPATCHER_TYPE)
   is
   begin
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      Resume(CONTROLLER_CLASS(routine), invoker);
      -- explicit *non* dispatch call, equivalent to
      --    Resume(CONTROLLER_TYPE(routine), invoker);

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   ------------
   -- Resume --
   ------------

   overriding procedure Resume
     (routine : in out COROUTINE_TYPE;
      invoker : in out COROUTINE_TYPE)
   is
   begin
      Resume(CONTROLLER_CLASS(routine), DISPATCHER_TYPE(invoker));
      -- dispatch call to Resume(COROUTINE_TYPE; DISPATCHER_TYPE)
      -- *not* a call to Resume(COROUTINE_CLASS; DISPATCHER_TYPE)
   end Resume;

   -----------
   -- Yield --
   -----------

   overriding procedure Yield
     (routine : in out COROUTINE_TYPE)
   is
      parent : CONTROLLER_TYPE renames CONTROLLER_TYPE(routine);
   begin
      parent.Yield;
   end Yield;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (routine : in out COROUTINE_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (routine.runner'Terminated);
   begin
      if routine.state /= DEAD then
         routine.Request_To_Exit;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- CoRoutine_Runner --
   ----------------------

   task body CoRoutine_Runner
   is
   begin
      routine.Commence;
      routine.main(routine);
      routine.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => routine.Quit(X);
   end CoRoutine_Runner;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
