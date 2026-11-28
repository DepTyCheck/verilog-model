-- Seed: 12361591299048812194,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity ua is
  port (tnlki : out std_logic; sdx : out real_vector(2 to 2); tlsqecnxbs : inout std_logic_vector(4 downto 4));
end ua;

architecture vdcofxdf of ua is
  
begin
  -- Single-driven assignments
  sdx <= sdx;
  
  -- Multi-driven assignments
  tlsqecnxbs <= tlsqecnxbs;
end vdcofxdf;

entity my is
  port (wiqkcju : in time; zvwpak : in bit; eovrurmi : linkage time_vector(1 downto 0));
end my;

library ieee;
use ieee.std_logic_1164.all;

architecture tzh of my is
  signal cjhinyz : std_logic_vector(4 downto 4);
  signal tjmfezt : real_vector(2 to 2);
  signal pvkizotl : std_logic;
begin
  aufuy : entity work.ua
    port map (tnlki => pvkizotl, sdx => tjmfezt, tlsqecnxbs => cjhinyz);
  
  -- Multi-driven assignments
  cjhinyz <= (others => '0');
end tzh;

entity vhhecnuo is
  port (bxu : linkage real; oqdqooi : in real; gr : buffer boolean; tome : out severity_level);
end vhhecnuo;

architecture apumqugbk of vhhecnuo is
  signal xbikabkcr : time_vector(1 downto 0);
  signal ab : bit;
  signal rjcfr : time;
begin
  r : entity work.my
    port map (wiqkcju => rjcfr, zvwpak => ab, eovrurmi => xbikabkcr);
  
  -- Single-driven assignments
  rjcfr <= 2 fs;
  gr <= gr;
  tome <= tome;
  ab <= '0';
end apumqugbk;



-- Seed after: 15470815559882833118,15306146742274979323
