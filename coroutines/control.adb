-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Exceptions;
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

   procedure Resume(self, target: in out BASE_CONTROLLER) is separate;

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

   procedure Cancel(self: in out BASE_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE) is
      use Ada.Exceptions;
      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      -- TODO: manage exceptions
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
