------------------------------------------------------------------------------
--  Hello world examples using Controllers
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Text_IO; use Ada.Text_IO;

with Control; use Control;

with Gotcha;

procedure test_ctrl
is
   dispatcher : DISPATCHER_TYPE;
begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Test 1 - Simple hello world
   ---------------------------------------------------------------------------
   Test_1:
   declare
      task type HELLO_RUN (controller: not null CONTROLLER_ACCESS);
      task body HELLO_RUN is
      begin
         controller.Commence;
         Put_Line("Test 1-Hello, world!");
         controller.Yield;
         -- closed here
         controller.Quit;
      exception
         when Exit_Controller => null;
         when X: others => controller.Quit(X); raise;
      end HELLO_RUN;

      hello_control : aliased CONTROLLER_TYPE;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      dispatcher.Resume(hello_control);
      hello_control.Request_To_Exit;
   end Test_1;

   ---------------------------------------------------------------------------
   --  Test 2 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_2:
   declare
      task type HELLO_RUN (
         controller : not null access CONTROLLER_TYPE'Class);
      task body HELLO_RUN is
      begin
         controller.Commence;
         Put_Line("Test 2-Hello, world!");
         controller.Quit;
      exception
         when X: others => controller.Quit(X); raise;
      end HELLO_RUN;

      type HELLO_COROUTINE is limited new CONTROLLER_TYPE with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      hello : HELLO_COROUTINE;
   begin
      dispatcher.Resume(hello);
   exception
      when Stop_Iteration => null; -- ignore
   end Test_2;

   ---------------------------------------------------------------------------
   --  Test 3 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_3:
   declare
      type HELLO_COROUTINE is tagged;

      task type HELLO_RUN (controller: not null access HELLO_COROUTINE);

      type HELLO_COROUTINE is limited new CONTROLLER_TYPE with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      task body HELLO_RUN is
      begin
         controller.Commence;
         Put_Line("Test 3-Hello, world!");
         controller.Quit;
      exception
         when X: others => controller.Quit(X); raise;
      end HELLO_RUN;

      hello : HELLO_COROUTINE;
   begin
      dispatcher.Resume(hello);
   exception
      when Stop_Iteration => null; -- ignore
   end Test_3;

   ---------------------------------------------------------------------------
   --  Test 4 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_4:
   declare
      package Hello_Package is
         type HELLO_COROUTINE is
            limited new CONTROLLER_TYPE with private;

         overriding
         procedure Quit(controller: in out HELLO_COROUTINE; X: EXCEPTION_TYPE);

         task type HELLO_RUN (
            controller: not null access CONTROLLER_TYPE'Class);

      private
         type HELLO_COROUTINE is limited new CONTROLLER_TYPE with
            record
               run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
            end record;
      end Hello_Package;

      package body Hello_Package is
         overriding
         procedure Quit(controller: in out HELLO_COROUTINE; X: EXCEPTION_TYPE)
         is
            super : CONTROLLER_TYPE
               renames CONTROLLER_TYPE(controller);
         begin
            super.Quit(X);
         end Quit;

         task body HELLO_RUN is
         begin
            controller.Commence;
            Put_Line("Test 4-Hello, world!");
            controller.Quit;
         exception
            when X: others => controller.Quit(X); raise;
         end HELLO_RUN;
      end Hello_Package;

      hello : Hello_Package.HELLO_COROUTINE;
   begin
      dispatcher.Resume(hello);
   exception
      when Stop_Iteration => null; -- ignore
   end Test_4;

   New_Line;

exception
   when Stop_Iteration => null; -- ignore
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_ctrl;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
