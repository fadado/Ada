------------------------------------------------------------------------------
--  Control . CoRoutines implementation (generic)
------------------------------------------------------------------------------

with Control.Spin_Until;

package body Control . CoRoutines is
   ---------------------------------------------------------------------------
   --  COROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   ----------
   -- Call --
   ----------

   Environment_Controller : CONTROLLER_TYPE;

   function Call
     (routine : in out COROUTINE_TYPE) return BOOLEAN
   is
   begin
      if routine.runner'Callable then
         Environment_Controller.Resume(CONTROLLER_TYPE(routine));
      end if;

      return routine.state /= DEAD;
   end Call;

   ------------
   -- Resume --
   ------------

   function Resume
     (routine : in out COROUTINE_TYPE;
      target  : in out COROUTINE_TYPE) return BOOLEAN
   is
      parent : CONTROLLER_TYPE renames CONTROLLER_TYPE(routine);
   begin
      if routine.runner'Callable then
         parent.Resume(CONTROLLER_TYPE(target));
      end if;

      return routine.state /= DEAD;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (routine : in out COROUTINE_TYPE)
   is
      parent : CONTROLLER_TYPE renames CONTROLLER_TYPE(routine);
   begin
      pragma Assert(routine.runner'Callable);
      parent.Yield;
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
      routine.Request_To_Exit;

      pragma Assert(routine.state = DEAD);

      Spin_Until(runner_terminated'Access);
   end Close;

   --------------------
   -- CoRoutine_Runner --
   --------------------

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
