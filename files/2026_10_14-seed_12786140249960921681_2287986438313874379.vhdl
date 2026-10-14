-- Seed: 12786140249960921681,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (x : out integer; mchecalmtr : inout character; fiechlwa : linkage std_logic_vector(3 downto 3));
end y;

architecture qc of y is
  
begin
  -- Single-driven assignments
  mchecalmtr <= mchecalmtr;
  x <= x;
end qc;



-- Seed after: 3367252267404063277,2287986438313874379
