-- hello.adb

pragma Restrictions (
   No_Select_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Abort_Statements
);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

procedure hello is

   procedure Report_Exception(X: EXCEPTION_OCCURRENCE; S: STRING) is
      msg : STRING := Exception_Message(X);
   begin
      Put_Line(Standard_Error, S);
      Put_Line(Standard_Error, Exception_Name(X));
      Put_Line(Standard_Error, Exception_Information(X));
      if msg /= "" then
         Put_Line(Standard_Error, msg);
      end if;
   end Report_Exception;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type HELLO_TASK_1(Hello: access CONTROLLER);

   task body HELLO_TASK_1 is
   begin
      Hello.Suspend;

      Put_Line("1-Hello, world!");

      Hello.Finish;
   exception
      when X: others =>
         Report_Exception(X, "Oops at HELLO_TASK_1!");
   end HELLO_TASK_1;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_2(Hello: access CONTROLLER);

   task body HELLO_TASK_2 is
   begin
      Hello.Suspend;

      Put_Line("2-Hello, world!");
   exception
      when X: others =>
         Report_Exception(X, "Oops at HELLO_TASK_2!");
   end HELLO_TASK_2;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type HELLO_TASK_3(Hello: access CONTROLLER);

   task body HELLO_TASK_3 is
   begin
      Hello.Suspend;

      Put("3-");      Hello.Yield;
      Put("Hello");   Hello.Yield;
      Put(", world"); Hello.Yield;
      Put_Line("!");

      Hello.Finish;
   exception
      when X: others =>
         Report_Exception(X, "Oops at HELLO_TASK_3!");
   end HELLO_TASK_3;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      main : CONTROLLER;
      hello_control : aliased CONTROLLER;
      hello_thread : HELLO_TASK_1 (hello_control'Access);
   begin
      main.Resume(hello_control);
   end;

   declare
      hello_control : aliased CONTROLLER;
      hello_thread : HELLO_TASK_2 (hello_control'Access);
   begin
      hello_control.Go;
   end;

   declare
      main : CONTROLLER;
      hello_control : aliased CONTROLLER;
      hello_thread : HELLO_TASK_3 (hello_control'Access);
   begin
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
   end;

end hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
