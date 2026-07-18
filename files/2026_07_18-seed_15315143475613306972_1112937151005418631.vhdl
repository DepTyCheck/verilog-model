-- Seed: 15315143475613306972,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity evumgo is
  port (patepm : out bit_vector(4 to 3); v : in std_logic_vector(0 to 0));
end evumgo;

architecture tmeuu of evumgo is
  
begin
  -- Single-driven assignments
  patepm <= (others => '0');
end tmeuu;

entity qbqni is
  port (qzw : out real);
end qbqni;

library ieee;
use ieee.std_logic_1164.all;

architecture aqvd of qbqni is
  signal qelfu : std_logic_vector(0 to 0);
  signal hthsqfqb : bit_vector(4 to 3);
  signal hcrlwve : std_logic_vector(0 to 0);
  signal pot : bit_vector(4 to 3);
  signal myko : bit_vector(4 to 3);
  signal bervsar : std_logic_vector(0 to 0);
  signal qxcs : bit_vector(4 to 3);
begin
  wbywvt : entity work.evumgo
    port map (patepm => qxcs, v => bervsar);
  idclhujm : entity work.evumgo
    port map (patepm => myko, v => bervsar);
  roveuz : entity work.evumgo
    port map (patepm => pot, v => hcrlwve);
  eidrs : entity work.evumgo
    port map (patepm => hthsqfqb, v => qelfu);
  
  -- Single-driven assignments
  qzw <= qzw;
  
  -- Multi-driven assignments
  qelfu <= hcrlwve;
  hcrlwve <= (others => 'X');
end aqvd;



-- Seed after: 17215419974947682647,1112937151005418631
