-- Seed: 5817402577160036562,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity ylltt is
  port (agwusi : out std_logic_vector(3 downto 4));
end ylltt;

architecture ricmrdavcn of ylltt is
  
begin
  -- Multi-driven assignments
  agwusi <= (others => '0');
  agwusi <= agwusi;
end ricmrdavcn;



-- Seed after: 10992513655974832921,13857275728440271305
