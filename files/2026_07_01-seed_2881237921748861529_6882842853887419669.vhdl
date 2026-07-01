-- Seed: 2881237921748861529,6882842853887419669

entity h is
  port (xegt : buffer integer);
end h;

architecture wbxl of h is
  
begin
  -- Single-driven assignments
  xegt <= 16#D_D_6_3#;
end wbxl;

library ieee;
use ieee.std_logic_1164.all;

entity obummbrmmv is
  port (yqijzfs : buffer std_logic; jfawrnqdq : in bit_vector(1 downto 0));
end obummbrmmv;

architecture yqq of obummbrmmv is
  signal jcsvaqejwx : integer;
  signal zabs : integer;
  signal yne : integer;
  signal lpddvu : integer;
begin
  iwhgvzoc : entity work.h
    port map (xegt => lpddvu);
  qnwiv : entity work.h
    port map (xegt => yne);
  ilzj : entity work.h
    port map (xegt => zabs);
  gz : entity work.h
    port map (xegt => jcsvaqejwx);
  
  -- Multi-driven assignments
  yqijzfs <= 'X';
end yqq;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (nvofx : buffer boolean_vector(1 to 3); iedgz : buffer boolean_vector(1 downto 2); byljbrxqaq : in std_logic);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture u of g is
  signal pomuoq : bit_vector(1 downto 0);
  signal i : std_logic;
  signal a : integer;
  signal tpwxsao : integer;
begin
  stbvws : entity work.h
    port map (xegt => tpwxsao);
  hnkkturnq : entity work.h
    port map (xegt => a);
  jq : entity work.obummbrmmv
    port map (yqijzfs => i, jfawrnqdq => pomuoq);
  
  -- Single-driven assignments
  iedgz <= (others => TRUE);
  pomuoq <= ('1', '0');
  nvofx <= (FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  i <= '-';
  i <= 'H';
  i <= 'L';
end u;



-- Seed after: 7167411368421939900,6882842853887419669
