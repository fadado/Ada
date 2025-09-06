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
   --  CONTROLLER_TYPE
   ---------------------------------------------------------------------------

   --------------
   -- Initiate --
   --------------

   procedure Initiate
     (controller : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert (controller.state = EXPECTANT);
      pragma Assert (controller.id = Null_Task_Id);

      controller.id := Current_Task;

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;

      pragma Assert (controller.link /= NULL);
   end Initiate;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (controller : in out CONTROLLER_TYPE)
   is
      back : DISPATCHER_TYPE renames controller.link.all;
   begin
      pragma Assert (controller.id = Current_Task);
      pragma Assert (controller.state = RUNNING);

      controller.Die; -- .state := DEAD
      Signal.Notify(back.run);
   end Quit;

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : EXCEPTION_TYPE)
   is
      use Ada.Exceptions;

      back : DISPATCHER_TYPE renames controller.link.all;
   begin
      pragma Assert (controller.state = RUNNING);

      back.migrant := Save_Occurrence(X);

      controller.Die; -- .state := DEAD
      Signal.Notify(back.run);
   end Quit;

   ---------
   -- Die --
   ---------

   procedure Die
     (controller : in out CONTROLLER_TYPE)
   is
   begin
      controller.id      := Null_Task_Id;
      controller.state   := DEAD;
      controller.link    := NULL;
      controller.migrant := NULL;

      pragma Assert (Signal.Is_Cleared(controller.run));
   end Die;

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
      Spin_Until(controller_initiated'Access);

      -- SUSPENDING
      dispatcher.state := SUSPENDED;
      Signal.Notify(controller.run);
      Signal.Wait(dispatcher.run);

      -- RESUMING
      if dispatcher.state = DYING then
         pragma Assert(not is_master_controller(dispatcher));
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
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert (controller.id = Current_Task);
      pragma Assert (controller.state = RUNNING);

      target.link := DISPATCHER_TYPE(controller)'Unchecked_Access;

      suspend_resume(DISPATCHER_TYPE(controller), target);

      if target.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE;
      suspend    : in BOOLEAN := TRUE)
   is
   begin
      pragma Assert (controller.id = Current_Task);
      pragma Assert (controller.state = RUNNING);

      target.link := controller.link;

      if suspend then
         suspend_resume(DISPATCHER_TYPE(controller), target);
      else
         Signal.Notify(target.run);
         -- the caller must Quit or raise Exit_Controller
      end if;
   end Transfer;

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
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;
   end Yield;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit
     (target : in out CONTROLLER_TYPE)
   is
      function target_died return BOOLEAN
         is (target.state = DEAD);
      function target_suspended return BOOLEAN
         is (target.state = SUSPENDED);
   begin
      pragma Assert (target.id /= Current_Task);

   <<again>>
      case target.state is
         when SUSPENDED =>
            target.state := DYING;
            Signal.Notify(target.run);
            Spin_Until(target_died'Access);
         when EXPECTANT =>
            Ada.Dispatching.Yield;
            goto again;
         when RUNNING =>
            Spin_Until(target_suspended'Access);
            goto again;
         when DEAD =>
            null;
         when DYING => -- cannot happen
            raise Control_Error;
      end case;
   end Request_To_Exit;

   ---------------------------------------------------------------------------
   --  DISPATCHER_TYPE
   ---------------------------------------------------------------------------

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

      if target.state = DEAD then
         raise Control_Error with "cannot resume dead controller";
      end if;

      target.link := dispatcher'Unchecked_Access;

      suspend_resume(dispatcher, target);

      if target.state = DEAD then
         raise Stop_Iteration;
      end if;
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
