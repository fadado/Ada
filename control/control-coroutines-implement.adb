------------------------------------------------------------------------------
--  Control . CoRoutines . Implement body (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . CoRoutines . Implement is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   ------------
   -- Resume -- TODO... rename to Dispatch?
   ------------

   not overriding procedure Resume
     (routine : in out COROUTINE_TYPE;
      invoker : in out DISPATCHER_TYPE)
   is
   begin
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      Control.Dispatch(routine, invoker);

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
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      Control.Dispatch(routine, DISPATCHER_TYPE(invoker));

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (routine : in out COROUTINE_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (routine.runner'Terminated);

      parent : BASE_CONTROLLER renames BASE_CONTROLLER(routine);
   begin
      if routine.state /= DEAD then
         parent.Close;
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
      routine.main(routine.all, routine.context);
      routine.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => routine.Quit(X);
   end CoRoutine_Runner;

end Control . CoRoutines . Implement;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
