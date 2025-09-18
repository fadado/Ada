------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Dispatching;

with Control.Spin_Until;

package body Control is

   function is_master_controller
     (dispatcher : DISPATCHER_TYPE'Class) return BOOLEAN
   with Inline
   is
      use type Ada.Tags.Tag;
   begin
      return DISPATCHER_TYPE'Tag = dispatcher'Tag;
   end is_master_controller;

   ---------------------------------------------------------------------------
   -- Low level primitives
   ---------------------------------------------------------------------------

   -- Check tests or coroutines and generators implementation to see how those
   -- low level primitives are used:
   --
   --    task type T_Runner (self: not null T_ACCESS);
   --
   --    task body T_Runner
   --    is
   --    begin
   --       self.Commence;
   --       ...
   --       ...
   --       self.Quit;
   --    exception
   --       when Exit_Controller => null;
   --       when X: others       => self.Quit(X);
   --    end T_Runner;

   --------------
   -- Commence --
   --------------

   procedure Commence
     (controller : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert (controller.id = Null_Task_Id);
      pragma Assert (controller.state = EXPECTANT);

      controller.id := Current_Task;

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         controller.STATE := DEAD;
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;

      -- we have been resumed!
      pragma Assert (controller.link /= NULL);
   end Commence;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : in EXCEPTION_TYPE := Null_Exception)
   is
      use Ada.Exceptions;

      invoker : DISPATCHER_TYPE renames controller.link.all;
   begin
      pragma Assert (controller.state = RUNNING);

      if Exception_Identity(X) /= Null_Id then
         invoker.migrant := Save_Occurrence(X);
      end if;

      controller.STATE := DEAD;
      Signal.Notify(invoker.run);
   end Quit;

   ---------------------------------------------------------------------------
   -- Public primitives
   ---------------------------------------------------------------------------

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit
     (dispatcher : in out DISPATCHER_TYPE)
   is
      function dispatcher_died return BOOLEAN
         is (dispatcher.state = DEAD);
    --function dispatcher_suspended return BOOLEAN
    --   is (dispatcher.state = SUSPENDED);
   begin
      pragma Assert (dispatcher.id /= Current_Task);

   <<again>>
      case dispatcher.state is
         when SUSPENDED =>
            dispatcher.state := DYING;
            Signal.Notify(dispatcher.run);
            Spin_Until(dispatcher_died'Access);
         when EXPECTANT =>
            Ada.Dispatching.Yield;
            goto again;
         when RUNNING =>   -- cannot happen?
            raise Control_Error;
          --Spin_Until(dispatcher_suspended'Access);
          --goto again;
         when DYING =>     -- cannot happen!
            raise Control_Error;
         when DEAD =>
            null;
      end case;
   end Request_To_Exit;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (controller : in out CONTROLLER_TYPE)
   is
      invoker : DISPATCHER_TYPE renames controller.link.all;
   begin
      pragma Assert (controller.id = Current_Task);
      pragma Assert (controller.state = RUNNING);

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Notify(invoker.run);
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         controller.STATE := DEAD;
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;
   end Yield;

   --------------------
   -- suspend_resume --
   --------------------

   procedure suspend_resume
     (dispatcher : in out DISPATCHER_TYPE;
      controller : in out CONTROLLER_TYPE)
   is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      procedure migrate_exception
      is
         use Ada.Exceptions;

         procedure dealloc is new Ada.Unchecked_Deallocation (
            EXCEPTION_TYPE,
            EXCEPTION_ACCESS
         );

         migrant : EXCEPTION_TYPE renames dispatcher.migrant.all;

         id : constant EXCEPTION_ID := Exception_Identity(migrant);
         ms : constant STRING       := Exception_Message(migrant);
      begin
         dealloc(dispatcher.migrant);
         dispatcher.migrant := NULL;
         Raise_Exception(id, ms);
      end migrate_exception;

      function controller_initiated return BOOLEAN
         is (controller.state /= EXPECTANT);
   begin
      if controller.state = DEAD then
         raise Control_Error with "cannot resume dead controller";
      end if;

      Spin_Until(controller_initiated'Access);

      -- SUSPENDING
      dispatcher.state := SUSPENDED;
      Signal.Notify(controller.run);
      Signal.Wait(dispatcher.run);

      -- RESUMING
      if dispatcher.state = DYING then
         pragma Assert (not is_master_controller(dispatcher));
         dispatcher.STATE := DEAD;
         raise Exit_Controller;
      end if;
      dispatcher.state := RUNNING;

      if dispatcher.migrant /= NULL then
         --  `controller` had an exception
         pragma Assert (controller.state = DEAD);

         migrate_exception;
      end if;
   end suspend_resume;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (dispatcher : in out DISPATCHER_TYPE;
      controller : in out CONTROLLER_TYPE'Class) 
   is
      target : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller);
   begin
      if dispatcher.id = Null_Task_Id then
         dispatcher.id    := Current_Task;
         dispatcher.state := RUNNING;
      end if;

      pragma Assert (dispatcher.id = Current_Task);
      pragma Assert (dispatcher.state = RUNNING);

      target.link := dispatcher'Unchecked_Access;

      suspend_resume(dispatcher, target);
   end Resume;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
      dispatcher : DISPATCHER_TYPE renames DISPATCHER_TYPE(controller);
   begin
      pragma Assert (controller.id = Current_Task);
      pragma Assert (controller.state = RUNNING);

      target.link := dispatcher'Unchecked_Access;

      suspend_resume(dispatcher, target);
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
