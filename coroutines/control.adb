------------------------------------------------------------------------------
--  Transfer of control (implementation)
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Unchecked_Deallocation;
with Ada.Real_Time;     

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Signals;                 use Signals;

package body Control is

   ---------------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------------

   ---------
   -- die --
   ---------

   procedure die(self: in out BASE_CONTROLLER'Class) is
   begin
      self.id    := Null_Task_Id;
      self.link  := NULL;
      self.state := DEAD;
      -- Clear(self.flag);
      -- self.migrant := NULL;
      -- self.xid := Null_Id;
   end die;

   -------------
   -- suspend --
   -------------

   procedure suspend(self: in out BASE_CONTROLLER'Class) is
   begin
      self.state := SUSPENDED;

      Wait(self.flag);

      if self.xid /= Null_Id then -- exit requested
         die(self);
         Raise_Exception(self.xid);
      end if;

      self.state := RUNNING;
   end suspend;

   ------------------
   -- init_if_head --
   ------------------

   procedure init_if_head(self: in out BASE_CONTROLLER'Class) is
   begin
      --  is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);

         self.id    := Current_Task;
         self.link  := self'Unchecked_Access; -- circular link
         self.state := RUNNING;
      end if;
   end init_if_head;

   ---------------
   -- spin_lock --
   ---------------

   procedure spin_lock(done: access function return BOOLEAN) is
      use Ada.Real_Time;

      msec : INTEGER := 100; -- are 100ms enough?
      stop : TIME := Clock + Milliseconds(msec);
   begin
      if not done.all then
         loop
            if Clock > stop then
               raise Program_Error with "loop timed out";
            end if;
            Ada.Dispatching.Yield;
            exit when done.all;
         end loop;
      end if;
   end spin_lock;

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

   ------------
   -- Status --
   ------------

   function Status(self: in BASE_CONTROLLER) return STATUS_TYPE is
   begin
      return self.state;
   end Status;

   ------------------
   -- Is_Yieldable --
   ------------------

   function Is_Yieldable(self: in BASE_CONTROLLER) return BOOLEAN is
   begin
      return self.id /= Null_Task_Id and then
             self.id /= Environment_Task;
   end Is_Yieldable;

   ------------
   -- Attach --
   ------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(self.state = SUSPENDED);

      self.id := Current_Task;
      suspend(self);

      pragma Assert(self.state = RUNNING);
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach(self: in out BASE_CONTROLLER) is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.state = RUNNING);

      die(self);
      Notify(back.flag);
   end Detach;

   ------------
   -- Resume --
   ------------

   procedure Resume(self, target: in out BASE_CONTROLLER) is
      use Ada.Exceptions;

      procedure dealloc is new Ada.Unchecked_Deallocation (
         EXCEPTION_OCCURRENCE,
         EXCEPTION_OCCURRENCE_ACCESS
      );
   begin
      pragma Assert(self.id = Current_Task);

      await_target_attach:
      declare
         function done return BOOLEAN is
            (target.id /= Null_Task_Id);
      begin
         spin_lock(done'Access);
      end await_target_attach;

      --  transfers control
      Notify(target.flag);
      suspend(self);

      --  check if target raised an exception!
      if self.migrant /= NULL then
         migrate_exception:
         declare
            id : EXCEPTION_ID renames Exception_Identity(self.migrant.all);
            ms : STRING       renames Exception_Message(self.migrant.all);
         begin
            dealloc(self.migrant);
            self.migrant := NULL;
            Raise_Exception(id, ms);
         end migrate_exception;
      end if;
   end Resume;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(self.state = SUSPENDED or else self.state = DEAD);

      if self.state = DEAD then
         null;
      elsif self.state = SUSPENDED then
         --  Exception `Exit_Controller` is only raised from here. Do not mask
         --  or map: to be handled only, masked, in the task body top handler.
         self.Throw(Exit_Controller'Identity);

         await_self_dead:
         declare
            function done return BOOLEAN is
               (self.state = DEAD);
         begin
            spin_lock(done'Access);
         end await_self_dead;
      else
         raise Program_Error;
      end if;
   end Close;

   -----------
   -- Throw --
   -----------

   procedure Throw(self: in out BASE_CONTROLLER; X: in EXCEPTION_ID) is
   begin
      pragma Assert(self.state = SUSPENDED);

      self.xid := X;
      Notify(self.flag);
   end Throw;

   -------------
   -- Migrate --
   -------------

   procedure Migrate(self: in out BASE_CONTROLLER; X: EXCEPTION_OCCURRENCE)
   is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);

      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      pragma Assert(self.id /= Null_Task_Id);
      pragma Assert(self.link /= NULL);

      -- Is `self` a head?
      if PTR'(self.link) = PTR'(self'Unchecked_Access) then
         raise Program_Error with "nowhere to migrate";
      end if;

      die(self);

      --  migrate exception occurrence
      back.migrant := Save_Occurrence(X);
      Notify(back.flag);
   end Migrate;

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   ------------
   -- Resume --
   ------------

   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      init_if_head(self);

      --  stack like linking
      target.link := self'Unchecked_Access;

      --  delegate to primary method
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER
                  renames ASYMMETRIC_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.state = RUNNING);

      Notify(invoker.flag);
      suspend(self);
   end Yield;

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   ------------
   -- Resume --
   ------------

   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      init_if_head(self);

      --  only can detach to the first controller
      target.link := self.link;

      --  delegate to primary method
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   ----------
   -- Jump --
   ----------

   --  Mandatory symmetric coroutines last call, except for the last to finish

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      pragma Assert(self.state = RUNNING);

      die(self);
      Notify(target.flag);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
