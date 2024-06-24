-- sieve.adb

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

use Ada;

with GNAT.OS_Lib;

procedure sieve is
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   subtype NUMBER is INTEGER range 1 .. INTEGER'Last;

   QUEUE_SIZE : constant := 2;

   package SQI is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => QUEUE_SIZE
      );

   subtype QUEUE is BSQ.Queue;
   type access_QUEUE is access QUEUE;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Odds_Generator (
      Output_Queue: access_QUEUE
   );
   type access_GENERATOR is access Odds_Generator;

   task body Odds_Generator is
      candidate : NUMBER := 3;
      -- start at the first odd prime
   begin
      loop
         Output_Queue.Enqueue(candidate);
         candidate := candidate + 2;
      end loop;
   end Odds_Generator;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Prime_Filter(
      Input_Queue  : access_QUEUE;
      Output_Queue : access_QUEUE;
      Prime        : NUMBER
   );
   type access_FILTER is access Prime_Filter;

   task body Prime_Filter is
      candidate : NUMBER;
   begin
      loop
         Input_Queue.Dequeue(candidate);
         if (candidate rem Prime) /= 0 then
            Output_Queue.Enqueue(candidate);
         end if;
      end loop;
   end Prime_Filter;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   Count : NATURAL := 0;

   procedure Print with Inline is
   begin
      Text_IO.New_Line;
   end;

   procedure Print(N: NUMBER) with Inline is
   begin
      Count := Count + 1;

      Integer_Text_IO.Put(N, Width => 7);
      if (Count rem 10) = 0 then Print; end if;
   end;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   procedure Main(Limit: NUMBER) is
      prime         : NUMBER;
      input, output : access_QUEUE;
      odds          : access_GENERATOR;
      layer         : access_FILTER;
   begin
      input := new QUEUE;
      odds  := new Odds_Generator (input);

      Print(2);
      loop
         input.Dequeue(prime);
         exit when prime > Limit;

         Print(prime);

         output := new QUEUE;
         layer  := new Prime_Filter (input, output, prime);
         input  := output;
      end loop;
      Print;
   end Main;

------------------------------------------------------------------------
--
------------------------------------------------------------------------
begin
   declare
      use Command_Line;

      LIMIT_100 : constant := 541; -- first 100 primes
      limit : NUMBER;
      last  : POSITIVE;
   begin
      if Argument_Count = 0 then
         limit := LIMIT_100;
      else
         begin
            Integer_Text_IO.Get(Argument(1), limit, last);
         exception
            when IO_Exceptions.Data_Error | Constraint_Error
               => return;
         end;
      end if;
      Main(Limit => limit);
   end;

   GNAT.OS_Lib.OS_Exit(0);
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
