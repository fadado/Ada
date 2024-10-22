-- test_hello.adb

pragma Assertion_Policy(Check); -- Check / Ignore

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

with Control;        use Control;
with Co_Op.Routines;

with Gotcha;

procedure test_hello is

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------
   --  Test 1 - Simple hello world
   ---------------------------------------------------------------------
   Test_1:
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

      main          : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      pragma Assert(not main.Is_Yieldable);
      pragma Assert(hello_control.Is_Yieldable);
      main.Resume(hello_control);
   end Test_1;

   ---------------------------------------------------------------------
   --  Test 2 - Asymmetric hello world
   ---------------------------------------------------------------------
   Test_2:
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

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      loop
         main.Resume(hello_control);
         exit when hello_control.Status = Control.DEAD;
         -- do something here...
      end loop;
   end Test_2;

   ---------------------------------------------------------------------
   --  Test 3 - Symmetric hello world
   ---------------------------------------------------------------------
   Test_3:
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

      main : aliased SYMMETRIC_CONTROLLER;
      hello_control : aliased SYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access, main'Unchecked_Access);
   begin
      loop
         main.Resume(hello_control);
         exit when hello_control.Status = Control.DEAD;
         -- do something here...
      end loop;
   end Test_3;

   ---------------------------------------------------------------------
   --  Test 4 - "Multiple" inheritance
   ---------------------------------------------------------------------
   Test_4:
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);
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

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   end Test_4;

   ---------------------------------------------------------------------
   --  Test 5 - "Multiple" inheritance
   ---------------------------------------------------------------------
   Test_5:
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

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   end Test_5;

   ---------------------------------------------------------------------
   --  Test 6 - "Multiple" inheritance
   ---------------------------------------------------------------------
   Test_6:
   declare
      package Hello_Package is
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with private;

         overriding
         procedure Cancel(self: in out HELLO_COROUTINE; X: Ada.Exceptions.EXCEPTION_OCCURRENCE);

         task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);

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

      main  : ASYMMETRIC_CONTROLLER;
      hello : Hello_Package.HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   end Test_6;

   ---------------------------------------------------------------------
   --  Test 7 - Co_Op example
   ---------------------------------------------------------------------

   Test_7:
   declare
      use Co_Op;

      package R is new Routines (CONTEXT_TYPE => NONE);

      procedure hello_world(self: R.ROUTINE_ACCESS; x: R.CONTEXT_ACCESS) is
      begin
         Put("Test 7-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
      end;

      hello : R.ROUTINE (hello_world'Access);

   begin
      loop hello.Resume; end loop;
   exception
      when Stop_Iteration => null;
   end Test_7;

   ---------------------------------------------------------------------
   --  Test 8 - Co_Op example
   ---------------------------------------------------------------------

   Test_8:
   declare
      use Co_Op;

      package R is new Routines (Context_Type => NONE);

      procedure hello_world(self: R.ROUTINE_ACCESS; x: R.CONTEXT_ACCESS) is
      begin
         Put("Test 8-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
      end;

      package routine is new R.Wrap (hello_world'Access);

   begin
      loop routine.Call; end loop;
   exception
      when Stop_Iteration => null;
   end Test_8;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
