-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Task_Identification;
with Ada.Dispatching;
with Ada.Real_Time;
with Signals;

use Ada.Task_Identification;
use Signals;

package body Control is

   ---------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------

   procedure reset(self: in out BASE_CONTROLLER) with Inline is
   begin
      pragma Assert(Is_Clean(self.flag));
      self.id := Null_Task_Id;
      self.invoker := NULL;
   end reset;

   procedure jump_and_die(self, target: in out BASE_CONTROLLER) with Inline is
   begin
      pragma Assert(self.invoker   /= NULL);
      pragma Assert(target.invoker /= NULL);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      Notify(target.flag);

      reset(self);
   end jump_and_die;

   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      self.id := Current_Task;
      Wait(self.flag);
      pragma Assert(self.invoker /= NULL);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      target : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
   begin
      jump_and_die(self, target);
   end Detach;

   procedure Resume(self, target: in out BASE_CONTROLLER) is
      -- internal subprograms
      procedure await_attach(co: in BASE_CONTROLLER) with Inline is
         use Ada.Real_Time;
         stop : TIME := Clock + Milliseconds(100);
      begin
         -- Ensure controller is attached
         if co.id = Null_Task_Id then
            loop
               if Clock > stop then
                  raise Program_Error with "loop timed out";
               end if;

               Ada.Dispatching.Yield;

               exit when co.id /= Null_Task_Id;
            end loop;
         end if;
         pragma Assert(co.id /= Current_Task);
      end await_attach;

      function is_asymmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in ASYMMETRIC_CONTROLLER'Class;
      end is_asymmetric;

      function is_symmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in SYMMETRIC_CONTROLLER'Class;
      end is_symmetric;

   begin
      pragma Assert(is_asymmetric(self) = is_asymmetric(target));

      if self.id = Null_Task_Id then
         -- initialize the main controller
         self.id := Current_Task;

         -- circular link
         self.invoker := self'Unchecked_Access;
      end if;

      pragma Assert(self.invoker /= NULL);
      pragma Assert(self.id = Current_Task);

      await_attach(target);
      pragma Assert(target.id /= Current_Task);

      if is_asymmetric(self) then
         target.invoker := self'Unchecked_Access;
      else
         target.invoker := self.invoker;
      end if;
      pragma Assert(target.invoker /= NULL);

      Notify(target.flag);
      Wait(self.flag);
   end Resume;

   procedure Kill(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(self.id /= Null_Task_Id);
      pragma Assert(self.id /= Current_Task);

      Abort_Task(self.id);

      reset(self);
   end Kill;

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      target : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      pragma Assert(self.invoker   /= NULL);
      pragma Assert(target.invoker /= NULL);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      Notify(target.flag);
      Wait(self.flag);
   end Yield;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      jump_and_die(BASE_CONTROLLER(self), BASE_CONTROLLER(target));
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
