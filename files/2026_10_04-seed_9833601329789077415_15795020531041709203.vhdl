-- Seed: 9833601329789077415,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ql is
  port (v : out std_logic; fimtghrmza : buffer time_vector(3 to 4));
end ql;

architecture pe of ql is
  
begin
  -- Single-driven assignments
  fimtghrmza <= (4 min, 4_3_4.4 ps);
  
  -- Multi-driven assignments
  v <= 'L';
  v <= '0';
  v <= 'Z';
end pe;



-- Seed after: 1989832637540301979,15795020531041709203
