------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Dispatching;

with Control.Spin_Until;

package body Control is

   ---------------------------------------------------------------------------
   -- DISPATCHER_TYPE local subprograms and public primitives
   ---------------------------------------------------------------------------

   --------------------------
   -- is_master_controller --
   --------------------------

   function is_master_controller
     (dispatcher : DISPATCHER_TYPE'Class) return BOOLEAN
   with Inline
   is
      use type Ada.Tags.Tag;
   begin
      return DISPATCHER_TYPE'Tag = dispatcher'Tag;
   end is_master_controller;

   --------------------
   -- suspend_resume --
   --------------------

   procedure suspend_resume
     (dispatcher : in out DISPATCHER_TYPE;
      target     : in out DISPATCHER_TYPE)
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
         is (target.state /= EXPECTANT);
   begin
      if target.state = DEAD then
         raise Control_Error with "cannot resume dead dispatcher";
      end if;

      Spin_Until(controller_initiated'Access);

      -- SUSPENDING
      dispatcher.state := SUSPENDED;
      Signal.Notify(target.run);
      Signal.Wait(dispatcher.run);

      -- RESUMING
      if dispatcher.state = DYING then
         pragma Assert (not is_master_controller(dispatcher));
         dispatcher.STATE := DEAD;
         raise Exit_Controller;
      end if;
      dispatcher.state := RUNNING;

      if dispatcher.migrant /= NULL then
         --  `target` had an exception
         pragma Assert (target.state = DEAD);

         migrate_exception;
      end if;
   end suspend_resume;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit
     (dispatcher : in out DISPATCHER_TYPE)
   is
      function dispatcher_died return BOOLEAN
         is (dispatcher.state = DEAD);
   begin
      pragma Assert (dispatcher.id /= Current_Task);
      pragma Assert (not is_master_controller(dispatcher));

   <<again>>
      case dispatcher.state is
         when SUSPENDED =>
            dispatcher.state := DYING;
            Signal.Notify(dispatcher.run);
            Spin_Until(dispatcher_died'Access);
         when EXPECTANT =>
            Ada.Dispatching.Yield;
            goto again;
         when DEAD =>
            null;
         when RUNNING | DYING =>
            raise Control_Error with "unexpected dispatcher state";
      end case;
   end Request_To_Exit;

   ---------------------------------------------------------------------------
   -- CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   -- To see how those low level primitives are used check tests or coroutines
   -- and generators implementation.
   --
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
         controller.state := DEAD;
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

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (controller : in out CONTROLLER_TYPE'Class;
      dispatcher : in out DISPATCHER_TYPE)
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

      suspend_resume(dispatcher, DISPATCHER_TYPE(target));
   end Dispatch;

   ---------------------------------------------------------------------------
   --  SEMI_
   ---------------------------------------------------------------------------

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

      suspend_resume(dispatcher, DISPATCHER_TYPE(target));
   end Resume;

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

   ---------------------------------------------------------------------------
   --  FULL_
   ---------------------------------------------------------------------------

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (dispatcher : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert (dispatcher.id = Current_Task);
      pragma Assert (dispatcher.state = RUNNING);

      target.link := dispatcher.link;

      suspend_resume(DISPATCHER_TYPE(dispatcher),
                     DISPATCHER_TYPE(target));
   end Transfer;

end Control;

-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
