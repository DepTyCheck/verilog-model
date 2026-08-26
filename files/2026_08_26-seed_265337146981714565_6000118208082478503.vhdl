-- Seed: 265337146981714565,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity qqt is
  port (lbkdu : buffer std_logic_vector(1 to 4); ch : inout std_logic_vector(0 to 1));
end qqt;

architecture va of qqt is
  
begin
  -- Multi-driven assignments
  ch <= ch;
  ch <= ch;
  ch <= ch;
  ch <= ch;
end va;

library ieee;
use ieee.std_logic_1164.all;

entity uhefvpff is
  port (i : in std_logic; mks : in std_logic_vector(1 to 2));
end uhefvpff;

library ieee;
use ieee.std_logic_1164.all;

architecture kw of uhefvpff is
  signal ep : std_logic_vector(0 to 1);
  signal sgfntgg : std_logic_vector(1 to 4);
begin
  foqr : entity work.qqt
    port map (lbkdu => sgfntgg, ch => ep);
  
  -- Multi-driven assignments
  sgfntgg <= ('L', 'Z', '0', '-');
  sgfntgg <= ('Z', 'Z', 'X', '1');
  sgfntgg <= "0W0X";
  ep <= ep;
end kw;



-- Seed after: 18141055978874517671,6000118208082478503
