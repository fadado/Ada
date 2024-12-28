------------------------------------------------------------------------------
--  Hello world examples using Controllers
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO; use Ada.Text_IO;

with Control.Routines;
use Control;

with Gotcha;

procedure test_hello is

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Test 1 - Simple hello world
   ---------------------------------------------------------------------------
   Test_1:
   declare
      pragma Warnings (Off, "unreachable code");

      task type HELLO_RUN (controller: not null CONTROLLER_ACCESS);
      task body HELLO_RUN is
      begin
         controller.Initiate;
         Put_Line("Test 1-Hello, world!");
         controller.Suspend;
         -- closed here
         controller.Quit;
      exception
         when Exit_Controller =>
            controller.Reset;
         when X: others =>
            controller.Quit(X);
            raise;
      end HELLO_RUN;

      main          : CONTROLLER_TYPE;
      hello_control : aliased CONTROLLER_TYPE;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      Put_Line(Standard_Error, "========================================================================");
      main.Call(hello_control);
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
         controller.Initiate;
         Put_Line("Test 2-Hello, world!");
         controller.Quit;
      exception
         when X: others => controller.Quit(X); raise;
      end HELLO_RUN;

      type HELLO_COROUTINE is limited new CONTROLLER_TYPE with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      main  : CONTROLLER_TYPE;
      hello : HELLO_COROUTINE;
   begin
      main.Call(CONTROLLER_TYPE(hello));
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
         controller.Initiate;
         Put_Line("Test 3-Hello, world!");
         controller.Quit;
      exception
         when X: others => controller.Quit(X); raise;
      end HELLO_RUN;

      main  : CONTROLLER_TYPE;
      hello : HELLO_COROUTINE;
   begin
      main.Call(CONTROLLER_TYPE(hello));
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
            controller.Initiate;
            Put_Line("Test 4-Hello, world!");
            controller.Quit;
         exception
            when X: others => controller.Quit(X); raise;
         end HELLO_RUN;
      end Hello_Package;

      main  : CONTROLLER_TYPE;
      hello : Hello_Package.HELLO_COROUTINE;
   begin
      main.Call(CONTROLLER_TYPE(hello));
   end Test_4;

   ---------------------------------------------------------------------------
   --  Test 5 - Routines example
   ---------------------------------------------------------------------------

   Test_5:
   declare
      pragma Warnings (Off, "unreachable code");

      package R is new Routines (Context_Type => VOID);
      use R;

      procedure subgen(routine: not null ROUTINE_ACCESS) is
      begin
         Put("He");
         routine.Yield;
         Put("llo");
         raise Stop_Iteration;
      end subgen;

      procedure hello_world(routine: not null ROUTINE_ACCESS) is
      begin
         Put("Test 5-");
         routine.Yield;
         begin subgen(routine);
         exception when Stop_Iteration => null; end;
         Put(", world");
         routine.Yield;
         Put_Line("!");
      end;

      hello : ROUTINE_TYPE (hello_world'Access, NULL);

   begin
      loop hello.Resume; end loop;
   exception
      when Stop_Iteration => null;
   end Test_5;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
