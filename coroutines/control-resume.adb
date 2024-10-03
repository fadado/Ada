-- control-resume.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;
with Ada.Task_Identification;
with Signals;

use Ada.Task_Identification;

separate (control)
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
      -- Is `target` in INACTIVE state (never resumed)?
      if target.id = Null_Task_Id then
         loop
            -- spin lock until `target.Attach` is called and blocks
            if Clock > stop then
               raise Program_Error with "loop timed out";
            end if;

            Ada.Dispatching.Yield;

            exit when target.id /= Null_Task_Id;
         end loop;
      end if;
      -- here `target` is BLOCKED
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
   --self.state := RUNNING;

   -- TODO: check if target raised an exception!
end Resume;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
