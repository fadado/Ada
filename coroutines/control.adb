------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;     
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Control.Spin_Until;

package body Control is

   use Ada.Task_Identification;

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

   -----------
   -- heads --
   -----------

   function heads(self: in out BASE_CONTROLLER'Class) return BOOLEAN
     with Inline
   is
      type POINTER is not null access all BASE_CONTROLLER'Class;
   begin
      return POINTER'(self.link) = POINTER'(self'Unchecked_Access);
   end heads;

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

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

   procedure Detach(self: in out BASE_CONTROLLER; X: EXCEPTION_TYPE)
   is
      use Ada.Exceptions;
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.state = RUNNING);

      if heads(self) then
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
         EXCEPTION_TYPE,
         EXCEPTION_ACCESS
      );
   begin
      --  is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);

         self.id    := Current_Task;
         self.link  := self'Unchecked_Access; -- circular link
         self.state := RUNNING;

         pragma Assert(heads(self));
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

      --  ensure `target` is attached
      Spin_Until(target_attached'Access);

      --  transfers control
      Signals.Notify(target.run);
      wait(self);

      --  `target` had one exception?
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
      pragma Assert(self.id /= Current_Task);

      if self.state = DEAD then
         null;
      elsif self.state = SUSPENDED then
         self.state := DYING;
         Signals.Notify(self.run);
         Spin_Until(have_died'Access);
      else
         raise Program_Error;
      end if;
   end Request_To_Exit;

   ---------
   -- Die --
   ---------

   procedure Die(self: in out BASE_CONTROLLER) is
   begin
      self.id      := Null_Task_Id;
      self.state   := DEAD;
      self.link    := NULL;
      self.migrant := NULL;
    --Signals.Clear(self.run);
   end Die;

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
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
