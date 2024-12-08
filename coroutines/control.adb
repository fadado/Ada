------------------------------------------------------------------------------
--  Control implementation
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;     
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Control.Spin_Until;

package body Control is

   use Ada.Task_Identification;

   ---------------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------------

   ----------
   -- wait --
   ----------

   procedure wait(controller: in out CONTROLLER_TYPE'Class) is
   begin
      controller.state := SUSPENDED;

      Signals.Wait(controller.run);

      if controller.state = DYING then
         -- obey received request to exit
         raise Exit_Controller;
      else
         controller.state := RUNNING;
      end if;
   end wait;

   ---------------
   -- is_master --
   ---------------

   function is_master(controller: in out CONTROLLER_TYPE'Class) return BOOLEAN
     with Inline
   is
      type POINTER is not null access all CONTROLLER_TYPE'Class;
   begin
      return POINTER'(controller.link)
               = POINTER'(controller'Unchecked_Access);
   end is_master;

   -------------
   -- migrate --
   -------------

   procedure migrate(controller: in out CONTROLLER_TYPE) is
      use Ada.Exceptions;

      procedure dealloc is new Ada.Unchecked_Deallocation (
         EXCEPTION_TYPE,
         EXCEPTION_ACCESS
      );

      id : EXCEPTION_ID := Exception_Identity(controller.migrant.all);
      ms : STRING       := Exception_Message(controller.migrant.all);
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

   procedure Initiate(controller: in out CONTROLLER_TYPE) is
   begin
      pragma Assert(controller.state = EXPECTANT);
      pragma Assert(controller.id = Null_Task_Id);

      controller.state := SUSPENDED;
      controller.id := Current_Task;
      wait(controller);

      pragma Assert(controller.state = RUNNING);
   end Initiate;

   ----------
   -- Quit --
   ----------

   procedure Quit(controller: in out CONTROLLER_TYPE) is
      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Reset;
      Signals.Notify(back.run);
   end Quit;

   procedure Quit(controller: in out CONTROLLER_TYPE; X: EXCEPTION_TYPE)
   is
      use Ada.Exceptions;
      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.state = RUNNING);

      if is_master(controller) then
         raise Program_Error with "nowhere to migrate";
      end if;

      back.migrant := Save_Occurrence(X);

      controller.Reset;
      Signals.Notify(back.run);
   end Quit;

   ----------
   -- Call --
   ----------

   procedure Call(controller, target: in out CONTROLLER_TYPE) is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_attached return BOOLEAN is
         (target.id /= Null_Task_Id);
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

      Spin_Until(target_attached'Access);

      Signals.Notify(target.run);
      wait(controller);

      if controller.migrant /= NULL then
         --  `target` had an exception
         pragma Assert(target.state = DEAD);
         migrate(controller);
      end if;
   end Call;

   --------------
   -- Transfer --
   --------------

   procedure Transfer(controller, target: in out CONTROLLER_TYPE) is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_attached return BOOLEAN is
         (target.id /= Null_Task_Id);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master(controller));

      target.link := controller.link;

      Spin_Until(target_attached'Access);

      Signals.Notify(target.run);
      wait(controller);

      if controller.migrant /= NULL then
         --  `target` had an exception
         pragma Assert(target.state = DEAD);
         migrate(controller);
      end if;
   end Transfer;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit(controller: in out CONTROLLER_TYPE)
   is
      function have_died return BOOLEAN is
         (controller.state = DEAD);
   begin
      pragma Assert(controller.id /= Current_Task);

      if controller.state = DEAD then
         null;
      elsif controller.state = SUSPENDED then
         controller.state := DYING;
         Signals.Notify(controller.run);
         Spin_Until(have_died'Access);
      else
         raise Program_Error;
      end if;
   end Request_To_Exit;

   -----------
   -- Reset --
   -----------

   procedure Reset(controller: in out CONTROLLER_TYPE) is
   begin
      controller.id      := Null_Task_Id;
      controller.state   := DEAD;
      controller.link    := NULL;
      controller.migrant := NULL;
    --Signals.Clear(controller.run);
   end Reset;

   -------------
   -- Suspend --
   -------------

   procedure Suspend(controller: in out CONTROLLER_TYPE) is
      invoker : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      Signals.Notify(invoker.run);
      wait(controller);
   end Suspend;

   ----------
   -- Jump --
   ----------

   procedure Jump(controller, target: in out CONTROLLER_TYPE) is
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Reset;
      Signals.Notify(target.run);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
