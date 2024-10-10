-- test_hello.adb

pragma Restrictions (
-- No_Abort_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Select_Statements
);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Control; use Control;
with Gotcha;
--with Co_Op;

procedure test_hello is

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------
   -- Test 1 - Simple hello world
   ---------------------------------------------------------------------
   declare
      pragma Warnings (Off, "unreachable code");

      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 1-Hello, world!");
         --raise Program_Error; -- handled with self.Cancel
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end HELLO_RUN;

      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- Test 2 - Asymmetric hello world
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put("Test 2-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end HELLO_RUN;

      head : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      loop
         head.Resume(hello_control);
         exit when hello_control.Detached;
         -- do something here...
      end loop;
   end;

   ---------------------------------------------------------------------
   -- Test 3 - Symmetric hello world
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self, other: not null access SYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
         invoker : SYMMETRIC_CONTROLLER renames other.all;
      begin
         self.Attach;
         Put("Test 3-"); self.Resume(invoker);
         Put("Hello");   self.Resume(invoker);
         Put(", world"); self.Resume(invoker);
         Put_Line("!");
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end HELLO_RUN;

      head : aliased SYMMETRIC_CONTROLLER;
      hello_control : aliased SYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access, head'Unchecked_Access);
   begin
      loop
         head.Resume(hello_control);
         exit when hello_control.Detached;
         -- do something here...
      end loop;
   end;

   ---------------------------------------------------------------------
   -- Test 4 - "Multiple" inheritance
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self: ASYMMETRIC_COROUTINE);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 4-Hello, world!");
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end HELLO_RUN;

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      hello : HELLO_COROUTINE;
   begin
      Resume(hello);
   end;

   ---------------------------------------------------------------------
   -- Test 5 - "Multiple" inheritance
   ---------------------------------------------------------------------
   declare
      type HELLO_COROUTINE is tagged;

      task type HELLO_RUN (self: not null access HELLO_COROUTINE);

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 5-Hello, world!");
         self.Detach;
      exception
         when X: others => self.Cancel(X); raise;
      end HELLO_RUN;

      hello : HELLO_COROUTINE;
   begin
      Resume(hello);
   end;

   ---------------------------------------------------------------------
   -- Test 6 - "Multiple" inheritance
   ---------------------------------------------------------------------
   declare
      package Hello_Package is
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with private;

         overriding
         procedure Cancel(self: in out HELLO_COROUTINE; X: Ada.Exceptions.EXCEPTION_OCCURRENCE);

         task type HELLO_RUN (self: ASYMMETRIC_COROUTINE);

      private
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
            record
               run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
            end record;
      end Hello_Package;

      package body Hello_Package is
         overriding
         procedure Cancel(self: in out HELLO_COROUTINE; X: Ada.Exceptions.EXCEPTION_OCCURRENCE) is
            super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
         begin
            super.Cancel(X);
            abort self.run; -- just in case
         end Cancel;

         task body HELLO_RUN is
         begin
            self.Attach;
            Put_Line("Test 6-Hello, world!");
            self.Detach;
         exception
            when X: others => self.Cancel(X); raise;
         end HELLO_RUN;
      end Hello_Package;

      hello : Hello_Package.HELLO_COROUTINE;
   begin
      hello.Resume;
   end;

   --test: raise Program_Error;
exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
