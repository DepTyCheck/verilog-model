-- Seed: 16692279591133414371,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (mpswfgs : out std_logic_vector(3 to 4); bjrm : out boolean_vector(4 to 1); alpkniewtw : linkage std_logic_vector(2 to 3));
end t;

architecture xidxplv of t is
  
begin
  -- Single-driven assignments
  bjrm <= (others => TRUE);
  
  -- Multi-driven assignments
  mpswfgs <= "U-";
  mpswfgs <= mpswfgs;
  mpswfgs <= mpswfgs;
end xidxplv;



-- Seed after: 8718160621777663575,13788294582928374589
