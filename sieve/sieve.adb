-- sieve.adb

with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

with GNAT.OS_Lib;

procedure sieve is
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   subtype NUMBER is INTEGER range 2 .. INTEGER'Last;

   package SQI is
      new Ada.Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Ada.Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => 1
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
      Ada.Text_IO.New_Line;
   end;

   procedure Print(N: NUMBER) with Inline is
   begin
      Count := Count + 1;

      Ada.Integer_Text_IO.Put(N, Width => 7);
      if (Count rem 10) = 0 then Print; end if;
   end;

------------------------------------------------------------------------
--
------------------------------------------------------------------------
begin
   declare
      LIMIT : constant := 999;

      prime         : NUMBER;
      input, output : access_QUEUE;
      generator     : access_GENERATOR;
      filter        : access_FILTER;
   begin
      input := new QUEUE;
      generator := new Odds_Generator (input);

      Print(2);
      loop
         input.Dequeue(prime);
         exit when prime > LIMIT;

         Print(prime);

         output := new QUEUE;
         filter := new Prime_Filter (input, output, prime);

         input := output;
      end loop;
      Print;

      GNAT.OS_Lib.OS_Exit(0);
   end;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
