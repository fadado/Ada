------------------------------------------------------------------------------
--  Control . CoRoutines implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   procedure Dispatch
     (routine    : in out COROUTINE_TYPE;
      dispatcher : in out DISPATCHER_TYPE)
   is
   begin
      dispatcher.Resume(routine);
   end Dispatch;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (routine : in out COROUTINE_TYPE;
      target  : in out COROUTINE_TYPE)
   is
   begin
      if target.state = DEAD then
         raise Control_Error with "cannot resume dead coroutine";
      end if;

      routine.Resume(CONTROLLER_TYPE(target));
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
      routine.Initiate;
      routine.main(routine);
      routine.Quit;
   exception
      when Exit_Controller => routine.Die;
      when X: others       => routine.Quit(X);
   end CoRoutine_Runner;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
