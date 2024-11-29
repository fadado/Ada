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

   procedure wait(controller: in out BASE_CONTROLLER'Class) is
   begin
      controller.state := SUSPENDED;

      Signals.Wait(controller.run);

      if controller.state = DYING then -- received request to exit
         raise Exit_Controller;
      else
         controller.state := RUNNING;
      end if;
   end wait;

   -----------
   -- heads --
   -----------

   function heads(controller: in out BASE_CONTROLLER'Class) return BOOLEAN
     with Inline
   is
      type POINTER is not null access all BASE_CONTROLLER'Class;
   begin
      return POINTER'(controller.link)
               = POINTER'(controller'Unchecked_Access);
   end heads;

   ---------------------------------------------------------------------------
   --  Base controller
   ---------------------------------------------------------------------------

   ------------
   -- Attach --
   ------------

   procedure Attach(controller: in out BASE_CONTROLLER) is
   begin
      pragma Assert(controller.state = EXPECTANT);
      pragma Assert(controller.id = Null_Task_Id);

      controller.state := SUSPENDED;
      controller.id := Current_Task;
      wait(controller);

      pragma Assert(controller.state = RUNNING);
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach(controller: in out BASE_CONTROLLER) is
      back : BASE_CONTROLLER renames BASE_CONTROLLER(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Die;
      Signals.Notify(back.run);
   end Detach;

   procedure Detach(controller: in out BASE_CONTROLLER; X: EXCEPTION_TYPE)
   is
      use Ada.Exceptions;
      back : BASE_CONTROLLER renames BASE_CONTROLLER(controller.link.all);
   begin
      pragma Assert(controller.state = RUNNING);

      if heads(controller) then
         raise Program_Error with "nowhere to migrate";
      end if;

      --  migrate exception occurrence
      back.migrant := Save_Occurrence(X);

      --  like simple detach
      controller.Die;
      Signals.Notify(back.run);
   end Detach;

   --------------
   -- Transfer --
   --------------

   procedure Transfer(controller, target: in out BASE_CONTROLLER) is
      use Ada.Exceptions;

      function target_attached return BOOLEAN is
         (target.id /= Null_Task_Id);

      procedure dealloc is new Ada.Unchecked_Deallocation (
         EXCEPTION_TYPE,
         EXCEPTION_ACCESS
      );
   begin
      --  is `controller` an uninitialized controller?
      if controller.id = Null_Task_Id then
         pragma Assert(controller.link = NULL);

         controller.id    := Current_Task;
         controller.link  := controller'Unchecked_Access; -- circular link
         controller.state := RUNNING;

         pragma Assert(heads(controller));
      end if;

      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      if BASE_CONTROLLER'Class(controller) in ASYMMETRIC_CONTROLLER then
         --  stack like linking
         target.link := controller'Unchecked_Access;
      elsif BASE_CONTROLLER'Class(controller) in SYMMETRIC_CONTROLLER then
         --  only can detach to the first controller
         target.link := controller.link;
      else
         raise Program_Error; -- no way
      end if;

      --  ensure `target` is attached
      Spin_Until(target_attached'Access);

      --  transfers control
      Signals.Notify(target.run);
      wait(controller);

      --  `target` had one exception?
      if controller.migrant /= NULL then
         pragma Assert(target.state = DEAD);
         declare
            id : EXCEPTION_ID := Exception_Identity(controller.migrant.all);
            ms : STRING       := Exception_Message(controller.migrant.all);
         begin
            dealloc(controller.migrant);
            controller.migrant := NULL;
            Raise_Exception(id, ms);
         end;
      end if;
   end Transfer;

   ---------------------
   -- Request_To_Exit --
   ---------------------

   procedure Request_To_Exit(controller: in out BASE_CONTROLLER)
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

   ---------
   -- Die --
   ---------

   procedure Die(controller: in out BASE_CONTROLLER) is
   begin
      controller.id      := Null_Task_Id;
      controller.state   := DEAD;
      controller.link    := NULL;
      controller.migrant := NULL;
    --Signals.Clear(controller.run);
   end Die;

   ---------------------------------------------------------------------------
   --  Asymmetric controller
   ---------------------------------------------------------------------------

   -------------
   -- Suspend --
   -------------

   procedure Suspend(controller: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER
         renames ASYMMETRIC_CONTROLLER(controller.link.all);
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      Signals.Notify(invoker.run);
      wait(controller);
   end Suspend;

   ---------------------------------------------------------------------------
   --  Symmetric controller
   ---------------------------------------------------------------------------

   ----------
   -- Jump --
   ----------

   procedure Jump(controller, target: in out SYMMETRIC_CONTROLLER) is
   begin
      pragma Assert(controller.id = Current_Task);
      pragma Assert(controller.state = RUNNING);

      controller.Die;
      Signals.Notify(target.run);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
