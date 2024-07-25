-- pingpong.adb

pragma Restrictions (No_Select_Statements);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

procedure pingpong is

   procedure Report_Exception(X: Exception_Occurrence) is
      msg : STRING := Exception_Message(X);
   begin
      Put_Line(Standard_Error, Exception_Name(X));
      Put_Line(Standard_Error, Exception_Information(X));
      if msg /= "" then
         Put_Line(Standard_Error, msg);
      end if;
   end Report_Exception;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_1(Hello: access CONTROLLER);

   task body Hello_Task_1 is
   begin
      Hello.Suspend;

      Put_Line("1-Hello, world!");

      Hello.Yield(Await => FALSE);
   exception
      when X: others =>
         Put_Line(Standard_Error, "Oops at Hello_Task_1!");
         Report_Exception(X);
         raise;
   end Hello_Task_1;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_2(Hello: access CONTROLLER);

   task body Hello_Task_2 is
   begin
      Hello.Suspend;

      Put_Line("2-Hello, world!");
   exception
      when X: others =>
         Put_Line(Standard_Error, "Oops at Hello_Task_2!");
         Report_Exception(X);
         raise;
   end Hello_Task_2;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_3(Hello: access CONTROLLER);

   task body Hello_Task_3 is
   begin
      Hello.Suspend;

      Put("3-");      Hello.Yield;
      Put("Hello");   Hello.Yield;
      Put(", world"); Hello.Yield;
      Put_Line("!");

      Hello.Yield(Await => FALSE);
   exception
      when X: others =>
         Put_Line(Standard_Error, "Oops at Hello_Task_3!");
         Report_Exception(X);
         raise;
   end Hello_Task_3;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Ping_Task(Ping, Pong: access CONTROLLER);
   task type Pong_Task(Pong, Ping: access CONTROLLER);

   task body Ping_Task is
   begin
      Ping.Suspend;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            Ping.Resume(Pong);
         end if;
      end loop;

      Pong.Resume;
   exception
      when X: others =>
         Put_Line(Standard_Error, "Oops at Ping_Task!");
         Report_Exception(X);
         raise;
   end Ping_Task;

   task body Pong_Task is
   begin
      Pong.Suspend;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            Pong.Resume(Ping);
         end if;
      end loop;

      Pong.Yield(Await => FALSE);
   exception
      when X: others =>
         Put_Line(Standard_Error, "Oops at Pong_Task!");
         Report_Exception(X);
         raise;
   end Pong_Task;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      main  : CONTROLLER;
      hello : aliased CONTROLLER;
      hello_thread : Hello_Task_1 (hello'Access);
   begin
      main.Resume(hello);
   end;

   declare
      hello : aliased CONTROLLER;
      hello_thread : Hello_Task_2 (hello'Access);
   begin
      hello.Resume;
   end;

   declare
      main  : CONTROLLER;
      hello : aliased CONTROLLER;
      hello_thread : Hello_Task_3 (hello'Access);
   begin
      main.Resume(hello);
      main.Resume(hello);
      main.Resume(hello);
      main.Resume(hello);
   end;

   declare
      main : CONTROLLER;
      ping : aliased CONTROLLER;
      pong : aliased CONTROLLER;
      ping_thread : Ping_Task (ping'Access, pong'Access);
      pong_thread : Pong_Task (pong'Access, ping'Access);
   begin
      New_Line;
      Put_Line("The players are ready...");
      New_Line;
      main.Resume(ping);
      New_Line;
      Put_Line("Game Over");
   end;

end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
