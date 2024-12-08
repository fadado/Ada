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
with Ada.Dispatching;

-- debugging only
with Ada.Text_IO;

package body Control is

   use Ada.Task_Identification;

   -- TODO: debug *deadlock*

   procedure trace(s: STRING) is
      use Ada.Text_IO;
   begin
      Put_Line(Standard_Error, s);
   end trace;

   ---------------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------------

   ----------
   -- wait --
   ----------

   procedure wait(controller: in out CONTROLLER_TYPE; caller: STRING:="") is
   begin
      trace(" >>WAIT "&caller&" ENTER");
      controller.state := SUSPENDED;

      trace(" >>WAIT "&caller&" SLEEP");
      Signal.Wait(controller.run);

      trace(" >>WAIT "&caller&" WAKEUP");

      if controller.state = DYING then
         trace(" >>WAIT "&caller&" 4");
         -- obey received request to exit
         raise Exit_Controller;
      else
         controller.state := RUNNING;
      end if;
   end wait;

   ---------------
   -- is_master --
   ---------------

   function is_master(controller: in out CONTROLLER_TYPE) return BOOLEAN
     with Inline
   is
      subtype NN is not null CONTROLLER_ACCESS;
   begin
      return NN(controller.link) = controller'Unchecked_Access;
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

      controller.id := Current_Task;
      wait(controller, "INITIATE");

      pragma Assert(controller.state = RUNNING);
   end Initiate;

   -------------
   -- Suspend --
   -------------

   procedure Suspend(controller: in out CONTROLLER_TYPE) is
      invoker : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      Signal.Notify(invoker.run);
      wait(controller, "SUSPEND");
   end Suspend;

   ----------
   -- Quit --
   ----------

   procedure Quit(controller: in out CONTROLLER_TYPE) is
      back : CONTROLLER_TYPE renames CONTROLLER_TYPE(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Reset;
      Signal.Notify(back.run);
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
      Signal.Notify(back.run);
   end Quit;

   -----------
   -- Reset --
   -----------

   procedure Reset(controller: in out CONTROLLER_TYPE) is
   begin
      controller.id      := Null_Task_Id;
      controller.state   := DEAD;
      controller.link    := NULL;
      controller.migrant := NULL;
    --Signal.Clear(controller.run);
   end Reset;

   ----------
   -- Call --
   ----------

   procedure Call(controller, target: in out CONTROLLER_TYPE) is
      use type Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS;

      function target_initiated return BOOLEAN is
         (target.state /= EXPECTANT);
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

      Signal.Notify(target.run);
      wait(controller, "CALL");

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

      function target_initiated return BOOLEAN is
         (target.state /= EXPECTANT);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);
      pragma Assert(not is_master(controller));

      target.link := controller.link;

      Spin_Until(target_initiated'Access);

      Signal.Notify(target.run);
      wait(controller);

      if controller.migrant /= NULL then
         --  `target` had an exception
         pragma Assert(target.state = DEAD);
         migrate(controller);
      end if;
   end Transfer;

   ----------
   -- Jump --
   ----------

   procedure Jump(controller, target: in out CONTROLLER_TYPE) is
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Reset;
      Signal.Notify(target.run);
   end Jump;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit(target: in out CONTROLLER_TYPE)
   is
      function have_died return BOOLEAN is
         (target.state = DEAD);
   begin
      trace("=>RTE");
      pragma Assert(target.id /= Current_Task);

      if target.state = DEAD then
         null;
      elsif target.state = SUSPENDED then
         -- HACK: this is not the problem !!! ???
         Ada.Dispatching.Yield;
         delay 0.001;
         delay 0.001;
         Ada.Dispatching.Yield;
         -- END

         target.state := DYING;
         trace("=>RTE Notify");
         Signal.Notify(target.run);
         Spin_Until(have_died'Access);
      else
         raise Program_Error;
      end if;
      trace("<=RTE");
   end Request_To_Exit;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
