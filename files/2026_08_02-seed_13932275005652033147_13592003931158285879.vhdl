-- Seed: 13932275005652033147,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (dbnddy : linkage std_logic_vector(2 to 2); py : out std_logic_vector(3 downto 2); u : linkage integer);
end e;

architecture deh of e is
  
begin
  -- Multi-driven assignments
  py <= ('Z', 'W');
  py <= py;
  py <= ('X', 'H');
  py <= "WU";
end deh;



-- Seed after: 13488311210811668537,13592003931158285879
