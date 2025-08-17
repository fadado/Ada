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

   --------------------------
   -- is_master_controller --
   --------------------------

   function is_master_controller
     (controller : in out CONTROLLER_TYPE) return BOOLEAN
   with Inline
   is
   begin
      return controller.link = controller'Unchecked_Access;
   end is_master_controller;

   -----------------------
   -- migrate_exception --
   -----------------------

   procedure migrate_exception
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
   end migrate_exception;

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

      pragma Assert(controller.link /= NULL);
      pragma Assert(not is_master_controller(controller));
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
      pragma Assert(not is_master_controller(controller));

      controller.Die;
      Signal.Notify(back.run);
   end Quit;

   procedure Quit
     (controller : in out CONTROLLER_TYPE;
      X          : EXCEPTION_TYPE)
   is
      use Ada.Exceptions;

      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master_controller(controller));

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
      pragma Assert(not is_master_controller(controller));

      controller.id      := Null_Task_Id;
      controller.state   := DEAD;
      controller.link    := NULL;
      controller.migrant := NULL;
    --Signal.Clear(controller.run);
   end Die;

   --------------------
   -- suspend_resume --
   --------------------

   procedure suspend_resume
     (controller : in out CONTROLLER_TYPE;
      target     : in out CONTROLLER_TYPE)
   with Inline
   is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_initiated return BOOLEAN
         is (target.state /= EXPECTANT);
   begin
      Spin_Until(target_initiated'Access);

      pragma Assert(not is_master_controller(target));

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

         migrate_exception(controller);
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
      if controller.id = Null_Task_Id then
         --  `controller` is an uninitialized master controller
         pragma Assert(controller.link = NULL);

         controller.id    := Current_Task;
         controller.state := RUNNING;
         controller.link  := controller'Unchecked_Access;
         -- circular link identifies master controllers

         pragma Assert(is_master_controller(controller));
      end if;

      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      target.link := controller'Unchecked_Access;

      suspend_resume(controller, target);
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
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master_controller(controller));

      target.link := controller.link;

      if suspend then
         suspend_resume(controller, target);
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
      invoker : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master_controller(controller));

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
      pragma Assert(target.id /= Current_Task);
      pragma Assert(not is_master_controller(target));

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
         when DYING =>
            raise Program_Error; -- cannot happen, but just in case...
      end case;
   end Request_To_Exit;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
