-- Seed: 16947500399403089223,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ol is
  port (uimn : linkage std_logic_vector(0 downto 0));
end ol;

architecture w of ol is
  
begin
  
end w;

entity kkv is
  port (qnx : buffer time);
end kkv;

library ieee;
use ieee.std_logic_1164.all;

architecture nre of kkv is
  signal xymxyfmc : std_logic_vector(0 downto 0);
  signal xrcbtda : std_logic_vector(0 downto 0);
begin
  zox : entity work.ol
    port map (uimn => xrcbtda);
  mn : entity work.ol
    port map (uimn => xymxyfmc);
  jrw : entity work.ol
    port map (uimn => xrcbtda);
  
  -- Single-driven assignments
  qnx <= 1 hr;
  
  -- Multi-driven assignments
  xrcbtda <= "Z";
  xymxyfmc <= xrcbtda;
  xrcbtda <= xrcbtda;
end nre;



-- Seed after: 250637397286644279,13501862637168280927
