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
   -- Resume -- TODO...
   ------------

   procedure Resume
     (self       : in out COROUTINE_TYPE;
      dispatcher : in out DISPATCHER_TYPE)
   is
   begin
      if self.state = DEAD then
         raise Stop_Iteration;
      end if;

      dispatcher.Dispatch(self);

      if self.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   ------------
   -- Resume --
   ------------

   overriding procedure Resume
     (self    : in out COROUTINE_TYPE;
      invoker : in out COROUTINE_TYPE)
   is
      dispatcher : DISPATCHER_TYPE renames DISPATCHER_TYPE(invoker);
   begin
      self.Resume(dispatcher);
   end Resume;

   -- Note: Yield is inherited from BASE_CONTROLLER

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (self : in out COROUTINE_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (self.runner'Terminated);

      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      if self.state /= DEAD then
         super.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- CoRoutine_Runner --
   ----------------------

   task body CoRoutine_Runner
   is
   begin
      self.Commence;
      self.main(self.all, self.context);
      self.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => self.Quit(X);
   end CoRoutine_Runner;

end Control . CoRoutines . Implement;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
