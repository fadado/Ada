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
         Ping.Transfer(Pong, suspend => (i < 10));
      end loop;

   exception
      when Exit_Controller => Ping.Die;
      when X: others       => Ping.Quit(X); raise;
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

   Environment_Controller : CONTROLLER_TYPE;

begin
   Gotcha.Set_Handlers;

   Test:
   declare
      ping_control : aliased CONTROLLER_TYPE;
      pong_control : aliased CONTROLLER_TYPE;
      ping_runner  : PING_RUN (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
      pong_runner  : PONG_RUN (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");

      Environment_Controller.Resume(ping_control);

      Put_Line("Game Over");
      New_Line;
   exception
      when X: others =>
         Gotcha.Report_Exception(X, "Oops at MASTER TASK!!");
   end Test;

end test_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
