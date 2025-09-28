------------------------------------------------------------------------------
--  Control . CoRoutines implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   procedure Resume
     (routine    : in out COROUTINE_TYPE;
      dispatcher : in out DISPATCHER_TYPE)
   is
   begin
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      CONTROLLER_TYPE'Class(routine).Resume(dispatcher);

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (routine : in out COROUTINE_TYPE;
      source  : in out COROUTINE_TYPE)
   is
      dispatcher : DISPATCHER_TYPE renames DISPATCHER_TYPE(source);
   begin
    --routine.Resume(dispatcher);
      --TODO ???
      --control-coroutines.adb:40:14: invalid procedure or entry call

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;

      CONTROLLER_TYPE'Class(routine).Resume(dispatcher);

      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (routine : in out COROUTINE_TYPE)
   is
      controller : CONTROLLER_TYPE renames CONTROLLER_TYPE(routine);
   begin
      controller.Yield;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close
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
