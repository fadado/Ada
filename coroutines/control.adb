------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Unchecked_Deallocation;
with Ada.Real_Time;     

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;

with Control.Spin_Until;

package body Control is
   ---------------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------------

   ----------
   -- wait --
   ----------

   procedure wait(self: in out BASE_CONTROLLER'Class) is
   begin
      self.state := SUSPENDED;

      Signals.Wait(self.run);

      if self.state = DYING then -- received request to exit
         raise Exit_Controller;
      else
         self.state := RUNNING;
      end if;
   end wait;

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

   ---------
   -- Die --
   ---------

   procedure Die(self: in out BASE_CONTROLLER) is
   begin
      self.id      := Null_Task_Id;
      self.state   := DEAD;
      self.link    := NULL;
      self.migrant := NULL;
      Signals.Clear(self.run);
   end Die;

   ------------
   -- Status --
   ------------

   function Status(self: in BASE_CONTROLLER) return STATUS_TYPE is
   begin
      return self.state;
   end Status;

   ------------
   -- Attach --
   ------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(self.state = EXPECTANT);
      pragma Assert(self.id = Null_Task_Id);

      self.state := SUSPENDED;
      self.id := Current_Task;
      wait(self);

      pragma Assert(self.state = RUNNING);
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach(self: in out BASE_CONTROLLER) is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.id = Current_Task);
      pragma Assert(self.state = RUNNING);

      self.Die;
      Signals.Notify(back.run);
   end Detach;

   --   Detach after an exception; called from an exception handler
   procedure Detach(self: in out BASE_CONTROLLER; X: EXCEPTION_OCCURRENCE)
   is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);

      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      -- Danger: Ada forbidens the call to `Current_Task` in handlers!!!
      --pragma Assert(self.id = Current_Task);
      pragma Assert(self.state = RUNNING);

      -- Is `self` a head?
      if PTR'(self.link) = PTR'(self'Unchecked_Access) then
         raise Program_Error with "nowhere to migrate";
      end if;

      --  migrate exception occurrence
      back.migrant := Save_Occurrence(X);

      --  like simple detach
      self.Die;
      Signals.Notify(back.run);
   end Detach;

   --------------
   -- Transfer --
   --------------

   procedure Transfer(self, target: in out BASE_CONTROLLER) is
      use Ada.Exceptions;

      function target_attached return BOOLEAN is
         (target.id /= Null_Task_Id);

      procedure dealloc is new Ada.Unchecked_Deallocation (
         EXCEPTION_OCCURRENCE,
         EXCEPTION_OCCURRENCE_ACCESS
      );
   begin
      --  is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);

         self.id    := Current_Task;
         self.link  := self'Unchecked_Access; -- circular link
         self.state := RUNNING;
      end if;

      pragma Assert(self.id = Current_Task);
      pragma Assert(self.state = RUNNING);

      if BASE_CONTROLLER'Class(self) in ASYMMETRIC_CONTROLLER then
         --  stack like linking
         target.link := self'Unchecked_Access;
      elsif BASE_CONTROLLER'Class(self) in SYMMETRIC_CONTROLLER then
         --  only can detach to the first controller
         target.link := self.link;
      else
         raise Program_Error; -- no way
      end if;

      --  ensure `target` suspends on `Attach`
      Spin_Until(target_attached'Access);

      --  transfers control
      Signals.Notify(target.run);
      wait(self);

      --  check if `target` detached with an exception!
      if self.migrant /= NULL then
         pragma Assert(target.state = DEAD);
         declare
            id : EXCEPTION_ID := Exception_Identity(self.migrant.all);
            ms : STRING       := Exception_Message(self.migrant.all);
         begin
            dealloc(self.migrant);
            self.migrant := NULL;
            Raise_Exception(id, ms);
         end;
      end if;
   end Transfer;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit(self: in out BASE_CONTROLLER)
   is
      function have_died return BOOLEAN is
         (self.state = DEAD);
   begin
      pragma Assert(self.state = SUSPENDED or else self.state = DEAD);
      pragma Assert(self.id /= Current_Task);

      if self.state = DEAD then
         null;
      elsif self.state = SUSPENDED then
         --  Exception `Exit_Controller` is only raised from here. Do not mask
         --  or map: to be handled only, masked, in the task body top handler.

         --  Send the request to the suspended controller and spin until
         --  received and processed
         self.state := DYING;
         Signals.Notify(self.run);
         Spin_Until(have_died'Access);
      else
         raise Program_Error;
      end if;
   end Request_To_Exit;

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   -------------
   -- Suspend --
   -------------

   procedure Suspend(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER
         renames ASYMMETRIC_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.id = Current_Task);
      pragma Assert(self.state = RUNNING);

      Signals.Notify(invoker.run);
      wait(self);
   end Suspend;

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   ----------
   -- Jump --
   ----------

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      pragma Assert(self.id = Current_Task);
      pragma Assert(self.state = RUNNING);

      self.Die;
      Signals.Notify(target.run);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
