------------------------------------------------------------------------------
--  Symmetric ping/pong
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

with Gotcha;

procedure test_pingpong
is
   task type PING_RUN(This, That: not null CONTROLLER_ACCESS);
   task type PONG_RUN(This, That: not null CONTROLLER_ACCESS);

   task body PING_RUN is
      Ping : CONTROLLER_TYPE renames This.all;
      Pong : CONTROLLER_TYPE renames That.all;
   begin
      Ping.Initiate;

      for i in 1..10 loop
         Put("PING!  ");
         --raise Program_Error;
         if i < 10 then
            Ping.Transfer(Pong);
         else
            Ping.Jump(Pong);
         end if;
      end loop;

   exception
      when Exit_Controller => Ping.Reset;
      when X: others => Ping.Quit(X); raise;
   end PING_RUN;

   task body PONG_RUN is
      Ping : CONTROLLER_TYPE renames This.all;
      Pong : CONTROLLER_TYPE renames That.all;
   begin
      Pong.Initiate;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            Pong.Transfer(Ping);
         else
            Pong.Quit;
         end if;
      end loop;

   exception
      when X: others => Pong.Quit(X); raise;
   end PONG_RUN;

begin
   Gotcha.Set_Handlers;

   Test:
   declare
      main         : CONTROLLER_TYPE;
      ping_control : aliased CONTROLLER_TYPE;
      pong_control : aliased CONTROLLER_TYPE;
      ping_runner  : PING_RUN (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
      pong_runner  : PONG_RUN (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");
      New_Line;

      main.Call(ping_control);

      New_Line;
      Put_Line("Game Over");
   exception
      when X: others =>
         Gotcha.Report_Exception(X, "Oops at MASTER TASK!!");
   end Test;

end test_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
