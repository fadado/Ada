-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Task_Identification;
with Signals;

use Ada.Task_Identification;

package body Control is
   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Resume(self, target: in out BASE_CONTROLLER) is separate;
   procedure Cancel(self: in out BASE_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE) is separate;

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      self.id := Current_Task;
      Signals.Wait(self.flag);

      pragma Assert(self.invoker /= NULL);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      invoker : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
   begin
      self.id := Null_Task_Id;
      self.invoker := NULL;
      Signals.Notify(invoker.flag);
   end Detach;

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      Signals.Notify(invoker.flag);
      Signals.Wait(self.flag);
   end Yield;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      self.id := Null_Task_Id;
      self.invoker := NULL;
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
