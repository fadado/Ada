------------------------------------------------------------------------------
--  Control . Routines implementation (generic)
------------------------------------------------------------------------------

with Control.Spin_Until;

package body Control . Routines is
   ---------------------------------------------------------------------------
   --  ROUTINE_TYPE methods
   ---------------------------------------------------------------------------

   ------------
   -- Resume --
   ------------

   procedure Resume(routine: in out ROUTINE_TYPE) is
   begin
      pragma Assert(routine.runner'Callable);

      routine.master.Transfer(ASYMMETRIC_CONTROLLER(routine));

      -- is routine detached?
      if routine.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(routine: in out ROUTINE_TYPE) is
   begin
      pragma Assert(routine.runner'Callable);
      routine.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(routine: in out ROUTINE_TYPE) is
      function runner_terminated return BOOLEAN is
         (routine.runner'Terminated);
   begin
      routine.Request_To_Exit;

      pragma Assert(routine.state = DEAD);

      Spin_Until(runner_terminated'Access);
   end Close;

   --------------------
   -- Routine_Runner --
   --------------------

   task body Routine_Runner is
   begin
      routine.Attach;
      routine.main(routine);
      routine.Detach;
   exception
      when Exit_Controller => routine.Die;
      when X: others       => routine.Detach(X);
   end Routine_Runner;

end Control . Routines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
