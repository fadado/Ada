------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Unchecked_Deallocation;
with Ada.Dispatching;

with Control.Spin_Until;

package body Control is

   ---------------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------------

   ---------------
   -- is_master --
   ---------------

   function is_master
     (controller : in out CONTROLLER_TYPE) return BOOLEAN
   with Inline
   is
   begin
      return controller.link = controller'Unchecked_Access;
   end is_master;

   -------------
   -- migrate --
   -------------

   procedure migrate
     (controller : in out CONTROLLER_TYPE)
   is
      use Ada.Exceptions;

      procedure dealloc is new Ada.Unchecked_Deallocation (
         EXCEPTION_TYPE,
         EXCEPTION_ACCESS
      );

      migrant : EXCEPTION_TYPE renames controller.migrant.all;

      id : constant EXCEPTION_ID := Exception_Identity(migrant);
      ms : constant STRING       := Exception_Message(migrant);
   begin
      dealloc(controller.migrant);
      controller.migrant := NULL;
      Raise_Exception(id, ms);
   end migrate;

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

   --------------
   -- Initiate --
   --------------

   procedure Initiate
     (controller : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert(controller.state = EXPECTANT);
      pragma Assert(controller.id = Null_Task_Id);

      controller.id := Current_Task;

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;
   end Initiate;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (controller : in out CONTROLLER_TYPE)
   is
      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Die;
      Signal.Notify(back.run);
   end Quit;

   procedure Quit
     (controller : in out CONTROLLER_TYPE; X: EXCEPTION_TYPE)
   is
      use Ada.Exceptions;

      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.state = RUNNING);

      if is_master(controller) then
         raise Program_Error with "nowhere to resume";
      end if;

      back.migrant := Save_Occurrence(X);

      controller.Die;
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
    --Signal.Clear(controller.run);
   end Die;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_initiated return BOOLEAN
         is (target.state /= EXPECTANT);
   begin
      if controller.id = Null_Task_Id then
         --  `controller` is an uninitialized master controller
         pragma Assert(controller.link = NULL);

         controller.id    := Current_Task;
         controller.state := RUNNING;
         controller.link  := controller'Unchecked_Access;
         -- circular link identifies master controllers

         pragma Assert(is_master(controller));
      end if;

      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      target.link := controller'Unchecked_Access;

      Spin_Until(target_initiated'Access);

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Notify(target.run);
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;

      if controller.migrant /= NULL then
         --  `target` had an exception
         pragma Assert(target.state = DEAD);
         migrate(controller);
      end if;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (controller : in out CONTROLLER_TYPE)
   is
      invoker : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

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

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_initiated return BOOLEAN
         is (target.state /= EXPECTANT);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master(controller));

      target.link := controller.link;

      Spin_Until(target_initiated'Access);

      -- SUSPENDING
      controller.state := SUSPENDED;
      Signal.Notify(target.run);
      Signal.Wait(controller.run);

      -- RESUMING
      if controller.state = DYING then
         raise Exit_Controller;
      end if;
      controller.state := RUNNING;

      if controller.migrant /= NULL then
         --  `target` had an exception
         pragma Assert(target.state = DEAD);
         migrate(controller);
      end if;
   end Transfer;

   ----------
   -- Jump --
   ----------

   procedure Jump
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   is
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      Signal.Notify(target.run);

      raise Exit_Controller;
   end Jump;

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
      pragma Assert(target.id /= Current_Task);

   <<again>>
      case target.state is
         when DEAD =>
            null;
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
         when DYING =>
            raise Program_Error;
      end case;
   end Request_To_Exit;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
