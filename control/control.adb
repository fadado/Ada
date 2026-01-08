------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Unchecked_Deallocation;
with Ada.Dispatching;

with Control.Spin_Until;

package body Control is

   procedure suspend_resume
     (source : in out DISPATCHER_TYPE;
      target : in out DISPATCHER_TYPE);

   ---------------------------------------------------------------------------
   -- DISPATCHER_TYPE local subprograms and public primitives
   ---------------------------------------------------------------------------

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (self       : in out DISPATCHER_TYPE;
      controller : in out CONTROLLER_TYPE'Class)
   is
      target : DISPATCHER_TYPE renames DISPATCHER_TYPE(controller);
   begin
      if self.id = Null_Task_Id then
         pragma Assert (self in DISPATCHER_TYPE);
         self.id    := Current_Task;
         self.state := RUNNING;
      end if;

      pragma Assert (self.id = Current_Task);
      pragma Assert (self.state = RUNNING);

      -- store link to invoker
      controller.backward := self'Unchecked_Access;

      suspend_resume(self, target);
   end Dispatch;

   ---------------------------------------------------------------------------
   -- CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   --------------
   -- Commence --
   --------------

   procedure Commence
     (self : in out CONTROLLER_TYPE)
   is
   begin
    --pragma Assert (self.id = Null_Task_Id);
    --pragma Assert (self.state = EXPECTANT);

      self.id := Current_Task;

      -- SUSPENDING
      self.state := SUSPENDED;
      Signal.Wait(self.run);

      -- RESUMING
      if self.state = DYING then
         self.state := DEAD;
         raise Exit_Controller;
      end if;
      self.state := RUNNING;

      -- we have been resumed by the invoker designated by `backward`
      pragma Assert (self.backward /= NULL);
   end Commence;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (self : in out CONTROLLER_TYPE;
      X    : in EXCEPTION_TYPE := Null_Exception)
   is
      use Ada.Exceptions;

      invoker : DISPATCHER_TYPE renames self.backward.all;
   begin
      pragma Assert (self.state = RUNNING);

      if Exception_Identity(X) /= Null_Id then
         invoker.migrant := Save_Occurrence(X);
      end if;

      self.state := DEAD;
      Signal.Notify(invoker.run);
   end Quit;

   -----------
   -- Close --
   -----------

   procedure Close
     (self : in out CONTROLLER_TYPE)
   is
      function dispatcher_died return BOOLEAN
         is (self.state = DEAD); -- .state is Atomic!
   begin
      pragma Assert (self.id /= Current_Task);

   <<again>>
      case self.state is
         when SUSPENDED =>
            self.state := DYING;
            Signal.Notify(self.run);
            Spin_Until(dispatcher_died'Access);
         when EXPECTANT =>
            Ada.Dispatching.Yield;
            goto again;
         when DEAD =>
            null;
         when RUNNING | DYING =>
            raise Control_Error with "unexpected dispatcher state";
      end case;
   end Close;

   ---------------------------------------------------------------------------
   --  SEMI_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   procedure Yield
     (self : in out SEMI_CONTROLLER_TYPE)
   is
      invoker : DISPATCHER_TYPE renames self.backward.all;
   begin
      -- too paranoid
    --pragma Assert (self.id = Current_Task);
    --pragma Assert (self.state = RUNNING);

      -- SUSPENDING
      self.state := SUSPENDED;
      Signal.Notify(invoker.run);
      Signal.Wait(self.run);

      -- RESUMING
      if self.state = DYING then
         self.state := DEAD;
         raise Exit_Controller;
      end if;
      self.state := RUNNING;
   end Yield;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (self       : in out SEMI_CONTROLLER_TYPE;
      controller : in out SEMI_CONTROLLER_TYPE)
   is
   begin
      controller.Dispatch(self);
   end Resume;

   ---------------------------------------------------------------------------
   --  FULL_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   -----------
   -- Yield -- TODO...
   -----------

   procedure Yield
     (self : in out FULL_CONTROLLER_TYPE)
   is
      master : DISPATCHER_TYPE renames self.backward.all;
   begin
      -- too paranoid
    --pragma Assert (self.id = Current_Task);
    --pragma Assert (self.state = RUNNING);

      pragma Assert (master in DISPATCHER_TYPE);

      -- SUSPENDING
      self.state := SUSPENDED;
      Signal.Notify(master.run);
      Signal.Wait(self.run);

      -- RESUMING
      if self.state = DYING then
         self.state := DEAD;
         raise Exit_Controller;
      end if;
      self.state := RUNNING;
   end Yield;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (self       : in out FULL_CONTROLLER_TYPE;
      controller : in out FULL_CONTROLLER_TYPE)
   is
      invoker : DISPATCHER_TYPE renames DISPATCHER_TYPE(controller);
      target  : DISPATCHER_TYPE renames DISPATCHER_TYPE(self);
   begin
      -- too paranoid
    --pragma Assert (controller.id = Current_Task);
    --pragma Assert (controller.state = RUNNING);

      -- store link to master
      self.backward := controller.backward;

      suspend_resume(invoker, target);
   end Resume;

   --------------------
   -- suspend_resume --
   --------------------

   procedure suspend_resume
     (source : in out DISPATCHER_TYPE;
      target : in out DISPATCHER_TYPE)
   is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      procedure migrate_exception
      is
         use Ada.Exceptions;

         procedure dealloc is new Ada.Unchecked_Deallocation (
            EXCEPTION_TYPE,
            EXCEPTION_ACCESS
         );

         migrant : EXCEPTION_TYPE renames source.migrant.all;

         id : constant EXCEPTION_ID := Exception_Identity(migrant);
         ms : constant STRING       := Exception_Message(migrant);
      begin
         dealloc(source.migrant);
         source.migrant := NULL;
         Raise_Exception(id, ms);
      end migrate_exception;

      function controller_initiated return BOOLEAN
         is (target.state /= EXPECTANT);
   begin
      if target.state = DEAD then
         raise Control_Error with "cannot resume dead dispatcher";
      end if;

      Spin_Until(controller_initiated'Access);

      -- SUSPENDING
      source.state := SUSPENDED;
      Signal.Notify(target.run);
      Signal.Wait(source.run);

      -- RESUMING
      if source.state = DYING then
         pragma Assert (source not in DISPATCHER_TYPE);
         source.state := DEAD;
         raise Exit_Controller;
      end if;
      source.state := RUNNING;

      if source.migrant /= NULL then
         --  `target` had an exception
         pragma Assert (target.state = DEAD);

         migrate_exception;
      end if;
   end suspend_resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
