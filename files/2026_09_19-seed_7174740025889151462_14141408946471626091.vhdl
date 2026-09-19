-- Seed: 7174740025889151462,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity kruavsgqsc is
  port (pf : out std_logic_vector(3 to 2));
end kruavsgqsc;

architecture lghvdbn of kruavsgqsc is
  
begin
  
end lghvdbn;

library ieee;
use ieee.std_logic_1164.all;

entity ju is
  port (b : out std_logic; j : inout std_logic_vector(3 to 2));
end ju;

library ieee;
use ieee.std_logic_1164.all;

architecture mpeqacl of ju is
  signal okw : std_logic_vector(3 to 2);
  signal kadqtkj : std_logic_vector(3 to 2);
begin
  fa : entity work.kruavsgqsc
    port map (pf => kadqtkj);
  djrqrsjogo : entity work.kruavsgqsc
    port map (pf => okw);
  
  -- Multi-driven assignments
  okw <= okw;
  j <= j;
end mpeqacl;



-- Seed after: 12791424163095505999,14141408946471626091
