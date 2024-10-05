-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Signals;

use Ada.Task_Identification;

package body Control is
   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      self.id := Current_Task;
      Signals.Wait(self.flag);
   end Attach;

   procedure Resume(self, target: in out BASE_CONTROLLER)
   is
      use Ada.Exceptions;
      procedure free is new Ada.Unchecked_Deallocation(
         Ada.Exceptions.EXCEPTION_OCCURRENCE,
         Ada.Exceptions.EXCEPTION_OCCURRENCE_ACCESS
      );

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
      pragma Assert(self.id = Current_Task);

      -- target must be active
      await_target_attach;
      pragma Assert(target.id /= Current_Task);

      -- transfers control
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

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      self.id := Null_Task_Id;
      self.invoker := NULL;
      Signals.Notify(invoker.flag);
   end Detach;

   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      -- is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.invoker = NULL);
         self.id := Current_Task;
         self.invoker := self'Unchecked_Access; -- circular link
      end if;

      -- stack like linking
      target.invoker := self'Unchecked_Access;

      -- dispatch
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      Signals.Notify(invoker.flag);
      Signals.Wait(self.flag);
   end Yield;

   procedure Cancel(self: in out ASYMMETRIC_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
   -- warning: cannot call Current_Task here!
   is
      use Ada.Exceptions;
      invoker : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
   begin
      pragma Assert(self.id /= Null_Task_Id);

      pragma Assert(self.invoker /= NULL);
      self.id := Null_Task_Id;
      Signals.Notify(invoker.flag);

      -- migrate exception
      self.invoker.migrant := Save_Occurrence(X);
   end Cancel;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self: in out SYMMETRIC_CONTROLLER) is
      head : SYMMETRIC_CONTROLLER renames SYMMETRIC_CONTROLLER(self.head.all);
   begin
      self.Detach(head);
   end Detach;

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      self.id := Null_Task_Id;
      self.head := NULL;
      Signals.Notify(target.flag);
   end Detach;

   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      if self.id = Null_Task_Id then
         pragma Assert(self.head = NULL);
         self.id := Current_Task;
         self.head := self'Unchecked_Access;
      end if;

      -- only can detach to the first controller
      target.head := self.head;

      -- dispatch
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   procedure Cancel(self: in out SYMMETRIC_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
   -- warning: cannot call Current_Task here!
   is
      use Ada.Exceptions;
      head : BASE_CONTROLLER renames BASE_CONTROLLER(self.head.all);
   begin
      pragma Assert(self.id /= Null_Task_Id);

      pragma Assert(self.head /= NULL);
      self.id := Null_Task_Id;
      Signals.Notify(head.flag);

      -- migrate exception
      self.head.migrant := Save_Occurrence(X);
   end Cancel;

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
