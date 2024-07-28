-- pingpong.adb

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

procedure pingpong is

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

   task type PING_TASK(Ping, Pong: access CONTROLLER);
   task type PONG_TASK(Pong, Ping: access CONTROLLER);

   task body PING_TASK is
   begin
      Ping.Suspend;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            Ping.Resume(Pong);
         end if;
      end loop;

      Pong.Go;
   exception
      when X: others =>
         Report_Exception(X, "Oops at PING_TASK!");
   end PING_TASK;

   task body PONG_TASK is
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
         Report_Exception(X, "Oops at PONG_TASK!");
   end PONG_TASK;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      main : CONTROLLER;
      ping_control : aliased CONTROLLER;
      pong_control : aliased CONTROLLER;
      ping_thread : PING_TASK (ping_control'Access, pong_control'Access);
      pong_thread : PONG_TASK (pong_control'Access, ping_control'Access);
   begin
      Put_Line("The players are ready...");
      New_Line;

      main.Resume(ping_control);

      New_Line;
      Put_Line("Game Over");
   end;

end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
