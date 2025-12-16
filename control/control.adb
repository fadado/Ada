------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Unchecked_Deallocation;
with Ada.Dispatching;

with Control.Spin_Until;

package body Control is

   ---------------------------------------------------------------------------
   -- DISPATCHER_TYPE local subprograms and public primitives
   ---------------------------------------------------------------------------

   procedure suspend_resume
     (dispatcher : in out DISPATCHER_TYPE;
      target     : in out DISPATCHER_TYPE);

   -----------
   -- Close --
   -----------

   procedure Close
     (dispatcher : in out DISPATCHER_TYPE)
   is
      function dispatcher_died return BOOLEAN
         is (dispatcher.state = DEAD); -- .state is Atomic!
   begin
      pragma Assert (dispatcher.id /= Current_Task);
      pragma Assert (dispatcher not in DISPATCHER_TYPE);

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
   end Close;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (dispatcher : in out DISPATCHER_TYPE;
      controller : in out CONTROLLER_TYPE'Class)
   is
      target : DISPATCHER_TYPE renames DISPATCHER_TYPE(controller);
   begin
      if dispatcher.id = Null_Task_Id then
         pragma Assert (dispatcher in DISPATCHER_TYPE);
         dispatcher.id    := Current_Task;
         dispatcher.state := RUNNING;
      end if;

      pragma Assert (dispatcher.id = Current_Task);
      pragma Assert (dispatcher.state = RUNNING);

      -- store link to invoker
      controller.backward := dispatcher'Unchecked_Access;

      suspend_resume(dispatcher, target);
   end Dispatch;

   ---------------------------------------------------------------------------
   -- CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   --------------
   -- Commence --
   --------------

   procedure Commence
     (controller : in out CONTROLLER_TYPE)
   is
   begin
    --pragma Assert (controller.id = Null_Task_Id);
    --pragma Assert (controller.state = EXPECTANT);

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

      -- we have been resumed by the invoker designated by `backward`
      pragma Assert (controller.backward /= NULL);
   end Commence;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : in EXCEPTION_TYPE := Null_Exception)
   is
      use Ada.Exceptions;

      invoker : DISPATCHER_TYPE renames controller.backward.all;
   begin
      pragma Assert (controller.state = RUNNING);

      if Exception_Identity(X) /= Null_Id then
         invoker.migrant := Save_Occurrence(X);
      end if;

      controller.state := DEAD;
      Signal.Notify(invoker.run);
   end Quit;

   ---------------------------------------------------------------------------
   --  SEMI_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   overriding procedure Yield
     (controller : in out SEMI_CONTROLLER_TYPE)
   is
      invoker : DISPATCHER_TYPE renames controller.backward.all;
   begin
      -- too paranoid
    --pragma Assert (controller.id = Current_Task);
    --pragma Assert (controller.state = RUNNING);

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Notify(invoker.run);
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         controller.state := DEAD;
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;
   end Yield;

   ------------
   -- Resume --
   ------------

   overriding procedure Resume
     (controller : in out SEMI_CONTROLLER_TYPE;
      dispatcher : in out SEMI_CONTROLLER_TYPE)
   is
   begin
      dispatcher.Dispatch(controller);
   end Resume;

   ---------------------------------------------------------------------------
   --  FULL_CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   -----------
   -- Yield -- TODO...
   -----------

   overriding procedure Yield
     (controller : in out FULL_CONTROLLER_TYPE)
   is
      master : DISPATCHER_TYPE renames controller.backward.all;
   begin
      -- too paranoid
    --pragma Assert (controller.id = Current_Task);
    --pragma Assert (controller.state = RUNNING);

      pragma Assert (master in DISPATCHER_TYPE);

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Notify(master.run);
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         controller.state := DEAD;
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;
   end Yield;

   ------------
   -- Resume --
   ------------

   overriding procedure Resume
     (controller : in out FULL_CONTROLLER_TYPE;
      dispatcher : in out FULL_CONTROLLER_TYPE)
   is
      invoker : DISPATCHER_TYPE renames DISPATCHER_TYPE(dispatcher);
      target  : DISPATCHER_TYPE renames DISPATCHER_TYPE(controller);
   begin
      -- too paranoid
    --pragma Assert (dispatcher.id = Current_Task);
    --pragma Assert (dispatcher.state = RUNNING);

      -- store link to master
      controller.backward := dispatcher.backward;

      suspend_resume(invoker, target);
   end Resume;

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
         pragma Assert (dispatcher not in DISPATCHER_TYPE);
         dispatcher.state := DEAD;
         raise Exit_Controller;
      end if;
      dispatcher.state := RUNNING;

      if dispatcher.migrant /= NULL then
         --  `target` had an exception
         pragma Assert (target.state = DEAD);

         migrate_exception;
      end if;
   end suspend_resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
