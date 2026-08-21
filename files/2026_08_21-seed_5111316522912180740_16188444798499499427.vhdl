-- Seed: 5111316522912180740,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity wp is
  port (gkaswpv : inout std_logic_vector(3 to 0));
end wp;

architecture zef of wp is
  
begin
  -- Multi-driven assignments
  gkaswpv <= (others => '0');
  gkaswpv <= gkaswpv;
  gkaswpv <= (others => '0');
  gkaswpv <= gkaswpv;
end zef;

library ieee;
use ieee.std_logic_1164.all;

entity gv is
  port (pedi : buffer std_logic; ur : in bit);
end gv;

library ieee;
use ieee.std_logic_1164.all;

architecture goathep of gv is
  signal fw : std_logic_vector(3 to 0);
  signal yhmdnxsxwh : std_logic_vector(3 to 0);
begin
  jvh : entity work.wp
    port map (gkaswpv => yhmdnxsxwh);
  gg : entity work.wp
    port map (gkaswpv => fw);
  fb : entity work.wp
    port map (gkaswpv => yhmdnxsxwh);
end goathep;



-- Seed after: 7394764073307699932,16188444798499499427
