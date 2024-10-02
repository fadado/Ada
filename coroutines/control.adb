-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;
with Ada.Task_Identification;
with Signals;

use Ada.Task_Identification;

package body Control is
   ---------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------

   procedure check_invariants(self, target: in out BASE_CONTROLLER) with Inline is
   begin
      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      pragma Assert(self.invoker   /= NULL);
      pragma Assert(target.invoker /= NULL);

      pragma Assert(Signals.Is_Clean(self.flag));
   end check_invariants;

   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      --self.state := INACTIVE;
      self.id := Current_Task;
      --self.state := BLOCKED;
      Signals.Wait(self.flag);
      --self.state := RUNNING;

      pragma Assert(self.invoker /= NULL);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      target : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
   begin
      check_invariants(self, target);

      self.id := Null_Task_Id;
      self.invoker := NULL;
      --self.state := INACTIVE;
      --target.state := RUNNING;
      Signals.Notify(target.flag);
   end Detach;

   procedure Cancel(self: in out BASE_CONTROLLER) is
      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      if self.id /= Null_Task_Id then
         self.id := Null_Task_Id;
         if self.invoker = NULL then
            null;
         elsif PTR'(self.invoker) = PTR'(self'Unchecked_Access) then
            -- a main controller
            self.invoker := NULL;
         elsif self.invoker /= NULL then
            declare
               target : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
            begin 
               self.invoker := NULL;
               --target.state := RUNNING;
               Signals.Notify(target.flag);
            end;
         end if;
         --self.state := BROKEN;
      end if;
   end Cancel;

   procedure Resume(self, target: in out BASE_CONTROLLER)
   is
      function is_asymmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in ASYMMETRIC_CONTROLLER'Class;
      end is_asymmetric;

      function is_symmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in SYMMETRIC_CONTROLLER'Class;
      end is_symmetric;

      procedure await_target_attach with Inline is
         use Ada.Real_Time;
         stop : TIME := Clock + Milliseconds(100);
      begin
         -- first resume for `target`?
         if target.id = Null_Task_Id then
            loop
               -- spin lock until `target.Attach` is called and blocks
               if Clock > stop then
                  raise Program_Error with "loop timed out";
               end if;

               Ada.Dispatching.Yield;

               exit when target.id /= Null_Task_Id;
            end loop;
            -- here `target` is BLOCKED
         end if;
      end await_target_attach;

   begin
      pragma Assert(is_asymmetric(self) = is_asymmetric(target));

      if self.id = Null_Task_Id then
         -- initialize a main controller
         self.id := Current_Task;

         -- circular link; this identifies the main controllers
         self.invoker := self'Unchecked_Access;
      end if;

      pragma Assert(self.id = Current_Task);
      pragma Assert(self.invoker /= NULL);

      await_target_attach;
      pragma Assert(target.id /= Current_Task);

      if is_asymmetric(self) then
         target.invoker := self'Unchecked_Access;
      else
         target.invoker := self.invoker;
      end if;
      pragma Assert(target.invoker /= NULL);

      check_invariants(self, target);

      --target.state := RUNNING;
      Signals.Notify(target.flag);
      --self.state := BLOCKED;
      Signals.Wait(self.flag);
   end Resume;

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      target : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      check_invariants(BASE_CONTROLLER(self), BASE_CONTROLLER(target));

      --target.state := RUNNING;
      Signals.Notify(target.flag);
      --self.state := BLOCKED;
      Signals.Wait(self.flag);
      --self.state := RUNNING;
   end Yield;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      check_invariants(BASE_CONTROLLER(self), BASE_CONTROLLER(target));

      self.id := Null_Task_Id;
      self.invoker := NULL;
      --self.state := INACTIVE;
      --target.state := RUNNING;
      Signals.Notify(target.flag);
   end Detach;

   ---------------------------------------------------------------------
   -- Syntactic sugar
   ---------------------------------------------------------------------

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER;
                    target: access ASYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Resume(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Detach(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) is
   begin
      Detach(self, target.all);
   end Detach;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
