-- Seed: 12961286631450728642,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity nh is
  port (oblj : inout integer; xpxl : linkage std_logic_vector(1 to 1));
end nh;

architecture nbyjcgdck of nh is
  
begin
  
end nbyjcgdck;

library ieee;
use ieee.std_logic_1164.all;

entity af is
  port (o : linkage std_logic_vector(4 downto 1));
end af;

library ieee;
use ieee.std_logic_1164.all;

architecture xnjllpehz of af is
  signal gdrskvaa : integer;
  signal pfgx : std_logic_vector(1 to 1);
  signal sp : integer;
  signal jxpxef : std_logic_vector(1 to 1);
  signal xukspru : integer;
begin
  b : entity work.nh
    port map (oblj => xukspru, xpxl => jxpxef);
  v : entity work.nh
    port map (oblj => sp, xpxl => pfgx);
  y : entity work.nh
    port map (oblj => gdrskvaa, xpxl => jxpxef);
  
  -- Multi-driven assignments
  jxpxef <= jxpxef;
  pfgx <= (others => '0');
  jxpxef <= "1";
  jxpxef <= jxpxef;
end xnjllpehz;

entity jhzmb is
  port (mdu : inout integer);
end jhzmb;

library ieee;
use ieee.std_logic_1164.all;

architecture dcqsaljcnw of jhzmb is
  signal yxq : std_logic_vector(4 downto 1);
  signal pghsfem : std_logic_vector(1 to 1);
  signal o : integer;
begin
  dfdierny : entity work.nh
    port map (oblj => o, xpxl => pghsfem);
  lbmz : entity work.af
    port map (o => yxq);
  nhjpqsu : entity work.nh
    port map (oblj => mdu, xpxl => pghsfem);
  
  -- Multi-driven assignments
  pghsfem <= "0";
  pghsfem <= pghsfem;
  pghsfem <= (others => 'L');
end dcqsaljcnw;



-- Seed after: 12367190192286139171,13843488114570579517
