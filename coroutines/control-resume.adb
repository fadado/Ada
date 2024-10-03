-- control-resume.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;

with Signals;

use Ada.Exceptions;
use Ada.Exceptions;
use Ada.Task_Identification;

separate (control)
procedure Resume(self, target: in out BASE_CONTROLLER)
is
   procedure free is new Ada.Unchecked_Deallocation(
       Ada.Exceptions.EXCEPTION_OCCURRENCE,
       Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS
   );

   function is_asymmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
   begin
      return BASE_CONTROLLER'Class(co) in ASYMMETRIC_CONTROLLER'Class;
   end is_asymmetric;

   function is_symmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
   begin
      return BASE_CONTROLLER'Class(co) in SYMMETRIC_CONTROLLER'Class;
   end is_symmetric;

   function is_head(co: in out BASE_CONTROLLER'Class) return BOOLEAN with Inline is
      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      return PTR'(co.invoker) = PTR'(co'Unchecked_Access);
   end is_head;

   procedure check_invariants with Inline is
   begin
      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      pragma Assert(self.invoker   /= NULL);
      pragma Assert(target.invoker /= NULL);

      pragma Assert(Signals.Is_Clean(self.flag));
   end check_invariants;

   procedure await_target_attach with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      -- Is `target` never resumed?
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

   -- is `self` an uninitialized head controller?
   if self.id = Null_Task_Id then
      pragma Assert(self.invoker = NULL);

      self.id := Current_Task;
      self.invoker := self'Unchecked_Access; -- circular link

      pragma Assert(is_head(self));
   end if;

   pragma Assert(self.id = Current_Task);
   pragma Assert(self.invoker /= NULL);

   await_target_attach;
   pragma Assert(target.id /= Current_Task);

   --   TODO...
   if is_asymmetric(self) then
      -- stack like linking
      target.invoker := self'Unchecked_Access;
   else
      -- preserve head
      pragma Assert(is_head(self.invoker.all));
      target.invoker := self.invoker;
   end if;
   pragma Assert(target.invoker /= NULL);

   check_invariants;

   Signals.Notify(target.flag);
   Signals.Wait(self.flag);

   -- check if target raised an exception!
   if self.migrant /= NULL then
      declare
         id : EXCEPTION_ID := Exception_Identity(self.migrant.all);
         ms : STRING := Exception_Message(self.migrant.all);
      begin
         --self.migrant := target.migrant;
         --free(target.migrant);
         Raise_Exception(id, ms);
      end;
   end if;
end Resume;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
