-- Seed: 13395370053820716475,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (cz : out std_logic_vector(3 to 3));
end w;

architecture ebmwg of w is
  
begin
  -- Multi-driven assignments
  cz <= "1";
end ebmwg;

library ieee;
use ieee.std_logic_1164.all;

entity gw is
  port (pygk : linkage std_logic);
end gw;

library ieee;
use ieee.std_logic_1164.all;

architecture jatoz of gw is
  signal ganlvddcc : std_logic_vector(3 to 3);
begin
  cz : entity work.w
    port map (cz => ganlvddcc);
  ktw : entity work.w
    port map (cz => ganlvddcc);
  
  -- Multi-driven assignments
  ganlvddcc <= (others => 'H');
  ganlvddcc <= ganlvddcc;
  ganlvddcc <= "Z";
end jatoz;

entity jcxmp is
  port (ggysqokekk : inout character; g : in time);
end jcxmp;

library ieee;
use ieee.std_logic_1164.all;

architecture ab of jcxmp is
  signal kxric : std_logic_vector(3 to 3);
  signal dg : std_logic_vector(3 to 3);
  signal kouudna : std_logic_vector(3 to 3);
begin
  geqix : entity work.w
    port map (cz => kouudna);
  ljd : entity work.w
    port map (cz => dg);
  yzpdidrjfi : entity work.w
    port map (cz => kxric);
  
  -- Single-driven assignments
  ggysqokekk <= ggysqokekk;
  
  -- Multi-driven assignments
  kxric <= kouudna;
  kouudna <= kouudna;
  kouudna <= "-";
end ab;



-- Seed after: 1598560198423763900,12260394286515585877
