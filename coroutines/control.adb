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
   end die;

   -------------
   -- suspend --
   -------------

   procedure suspend(self: in out BASE_CONTROLLER'Class) is
   begin
      self.state := SUSPENDED;

      Wait(self.flag);

      if self.state /= DYING then
         self.state := RUNNING;
      else -- exit requested
         die(self);
         raise Exit_Controller;
      end if;
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

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

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

   procedure Resume(self, target: in out BASE_CONTROLLER)
   is
      use Ada.Exceptions;

      -------------------------
      -- await_target_attach --
      -------------------------

      procedure await_target_attach with Inline is
         use Ada.Real_Time;
         stop : TIME := Clock + Milliseconds(100);
      begin
         --  Is `target` never resumed?
         if target.id = Null_Task_Id then
            --  spin lock until `target.Attach` is called
            loop
               if Clock > stop then
                  raise Program_Error with "loop timed out";
               end if;
               Ada.Dispatching.Yield;
               exit when target.id /= Null_Task_Id;
            end loop;
         end if;
      end await_target_attach;

      -----------------------
      -- migrate_exception --
      -----------------------

      procedure migrate_exception with Inline is
         ----------
         -- free --
         ----------

         procedure free is new Ada.Unchecked_Deallocation(
            EXCEPTION_OCCURRENCE,
            EXCEPTION_OCCURRENCE_ACCESS
         );

         id : EXCEPTION_ID renames Exception_Identity(self.migrant.all);
         ms : STRING       renames Exception_Message(self.migrant.all);

      begin
         free(self.migrant);
         self.migrant := NULL;
         Raise_Exception(id, ms);
      end migrate_exception;

   begin -- Resume
      pragma Assert(self.id = Current_Task);

      --  target must be active
      await_target_attach;
      pragma Assert(target.id /= Current_Task);

      --  transfers control
      Notify(target.flag);
      suspend(self);

      --  check if target raised an exception!
      if self.migrant /= NULL then
         migrate_exception;
      end if;
   end Resume;

   -------------
   -- Migrate --
   -------------

   --  warning: cannot call `Current_Task` here!

   procedure Migrate(self: in out BASE_CONTROLLER; X: EXCEPTION_OCCURRENCE)
   is
      -------------
      -- is_head --
      -------------

      function is_head(co: in out BASE_CONTROLLER'Class) return BOOLEAN
        with Inline
      is
         type PTR is not null access all BASE_CONTROLLER'Class;
      begin
         return PTR'(co.link) = PTR'(co'Unchecked_Access);
      end is_head;

      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);

   begin -- Migrate
      pragma Assert(self.id /= Null_Task_Id);
      pragma Assert(self.link /= NULL);

      if is_head(self) then
         raise Program_Error with "nowhere to migrate";
      end if;

      die(self);

      --  migrate exception occurrence
      back.migrant := Save_Occurrence(X);
      Notify(back.flag);
   end Migrate;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(self.state = SUSPENDED or else self.state = DEAD);

      if self.state = SUSPENDED then
         self.state := DYING;
         Notify(self.flag);
      elsif self.state = DEAD then
         null;
      else
         raise Program_Error;
      end if;
   end Request_To_Exit;

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
