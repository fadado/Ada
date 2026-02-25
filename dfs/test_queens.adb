-- Solves the
--    https://en.wikipedia.org/wiki/Eight_queens_puzzle

pragma Assertion_Policy(Ignore); -- Check / Ignore

with Ada.Text_IO;

with Depth_First_Search;

procedure test_queens is

   generic
      BOARD_SIZE : INTEGER := 8;
   procedure Queens_Solver;

   procedure Queens_Solver
   is
      pragma Assert (BOARD_SIZE >= 4);

      subtype COLUMN_INDEX is INTEGER range 1..BOARD_SIZE;
      subtype ROW_INDEX    is INTEGER range 1..BOARD_SIZE;
      type CHESS_BOARD     is array (ROW_INDEX) of COLUMN_INDEX;

      subtype LESS_DIAGONAL_ID is INTEGER
         range ROW_INDEX'First - ROW_INDEX'Last
            .. ROW_INDEX'Last  - ROW_INDEX'First;
      subtype PLUS_DIAGONAL_ID is INTEGER
         range ROW_INDEX'First + ROW_INDEX'First
            .. ROW_INDEX'Last  + ROW_INDEX'Last;

      Used_Column   : array (COLUMN_INDEX) of BOOLEAN := (others => FALSE);
      Less_Diagonal : array (ROW_INDEX) of LESS_DIAGONAL_ID;
      Plus_Diagonal : array (ROW_INDEX) of PLUS_DIAGONAL_ID;

      procedure Goal(board: in CHESS_BOARD)
      is
         use Ada.Text_IO;
      begin
         for column of board loop
            Put(column'Image);
         end loop;
         New_Line;
      end Goal;

      function Rejected
      (board : in CHESS_BOARD;
         row   : in ROW_INDEX;
         col   : in COLUMN_INDEX) return BOOLEAN
      with
         Pre => (row > ROW_INDEX'First)
      is
      begin
         if Used_Column(col) then
            return TRUE;
         end if;

         declare
            less_id : constant LESS_DIAGONAL_ID
               := LESS_DIAGONAL_ID(INTEGER(row) - INTEGER(col));
         begin
            if (for some r in ROW_INDEX'First .. ROW_INDEX'Pred(row)
                  => less_id = Less_Diagonal(r))
            then
               return TRUE;
            end if;
         end;

         declare
            plus_id : constant PLUS_DIAGONAL_ID
               := PLUS_DIAGONAL_ID(INTEGER(row) + INTEGER(col));
         begin
            if (for some r in ROW_INDEX'First .. ROW_INDEX'Pred(row)
                  => plus_id = Plus_Diagonal(r))
            then
               return TRUE;
            end if;
         end;

         return FALSE;
      end;

      procedure Enter
      (board : in CHESS_BOARD;
         row   : in ROW_INDEX;
         col   : in COLUMN_INDEX)
      is
      begin
         Used_Column(col) := TRUE;
         Less_Diagonal(row) := LESS_DIAGONAL_ID(INTEGER(row) - INTEGER(col));
         Plus_Diagonal(row) := PLUS_DIAGONAL_ID(INTEGER(row) + INTEGER(col));
      end;

      procedure Leave
      (board : in CHESS_BOARD;
         row   : in ROW_INDEX;
         col   : in COLUMN_INDEX) with Inline
      is
      begin
         Used_Column(col) := FALSE;
      end;

      package QueensDFS is
         new Depth_First_Search (
           ARRAY_TYPE   => CHESS_BOARD,
           INDEX_TYPE   => ROW_INDEX,
           ELEMENT_TYPE => COLUMN_INDEX
         );
   begin
      QueensDFS.Seek;
   end Queens_Solver;

   procedure Solver is new Queens_Solver(4);
begin
      Solver;
end test_queens;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
