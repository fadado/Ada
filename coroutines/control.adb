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

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure kill(self: in out BASE_CONTROLLER'Class) is
   begin
      -- TODO: rename as `Close`? abort_task self.id?
      self.id    := Null_Task_Id;
      self.link  := NULL;
      self.state := DEAD;
   end kill;

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
      Wait(self.flag);
      self.state := RUNNING;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach(self: in out BASE_CONTROLLER) is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      kill(self);
      Notify(back.flag);
   end Detach;

   ------------
   -- Resume --
   ------------

   procedure Resume(self, target: in out BASE_CONTROLLER)
   is
      use Ada.Exceptions;

      ----------
      -- free --
      ----------

      procedure free is new Ada.Unchecked_Deallocation(
         EXCEPTION_OCCURRENCE,
         EXCEPTION_OCCURRENCE_ACCESS
      );

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
         id : EXCEPTION_ID := Exception_Identity(self.migrant.all);
         ms : STRING := Exception_Message(self.migrant.all);
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
      self.state := NORMAL;
      Notify(target.flag);
      Wait(self.flag);
      self.state := RUNNING;

      --  check if target raised an exception!
      if self.migrant /= NULL then
         migrate_exception;
      end if;
   end Resume;

   ------------
   -- Cancel --
   ------------

   --  warning: cannot call `Current_Task` here!

   procedure Cancel(self: in out BASE_CONTROLLER; X: EXCEPTION_OCCURRENCE)
   is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);

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

   begin -- Cancel
      pragma Assert(self.id /= Null_Task_Id);
      pragma Assert(self.link /= NULL);

      if is_head(self) then
         raise Program_Error with "nowhere to migrate";
      end if;

      kill(self);

      --  migrate exception occurrence
      back.migrant := Save_Occurrence(X);
      Notify(back.flag);
   end Cancel;

   ------------
   -- Status --
   ------------

   function Status(self: in out BASE_CONTROLLER) return STATUS_TYPE is
   begin
      return self.state;
   end Status;

   ------------------
   -- Is_Yieldable --
   ------------------

   function Is_Yieldable(self: in out BASE_CONTROLLER) return BOOLEAN is
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
      --  is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);
         self.id := Current_Task;
         self.link := self'Unchecked_Access; -- circular link
      end if;

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
      self.state := SUSPENDED;
      Notify(invoker.flag);
      Wait(self.flag);
      self.state := RUNNING;
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
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);
         self.id := Current_Task;
         self.link := self'Unchecked_Access;
      end if;

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
      kill(self);
      Notify(target.flag);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
