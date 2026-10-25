-- Seed: 11821933708557665302,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity cofatfla is
  port (mnrrrlwyrt : linkage std_logic; siszwaok : linkage std_logic; h : inout string(1 downto 2));
end cofatfla;

architecture mtdmw of cofatfla is
  
begin
  -- Single-driven assignments
  h <= h;
end mtdmw;

entity odheh is
  port (r : in bit);
end odheh;

library ieee;
use ieee.std_logic_1164.all;

architecture sflmx of odheh is
  signal pfydl : string(1 downto 2);
  signal ursrbugdrq : std_logic;
  signal yqyvgbff : string(1 downto 2);
  signal wxfesrcm : std_logic;
  signal vlewyumum : std_logic;
  signal lsxhmb : string(1 downto 2);
  signal naai : std_logic;
  signal rh : std_logic;
begin
  zrjah : entity work.cofatfla
    port map (mnrrrlwyrt => rh, siszwaok => naai, h => lsxhmb);
  xzzwxay : entity work.cofatfla
    port map (mnrrrlwyrt => vlewyumum, siszwaok => wxfesrcm, h => yqyvgbff);
  vpia : entity work.cofatfla
    port map (mnrrrlwyrt => ursrbugdrq, siszwaok => vlewyumum, h => pfydl);
  
  -- Multi-driven assignments
  rh <= wxfesrcm;
end sflmx;

library ieee;
use ieee.std_logic_1164.all;

entity cmcv is
  port (go : linkage character; vhsbzr : linkage bit; rcmkmks : out std_logic_vector(2 to 0); irw : buffer character);
end cmcv;

architecture afugef of cmcv is
  signal hovi : bit;
begin
  rn : entity work.odheh
    port map (r => hovi);
  
  -- Single-driven assignments
  hovi <= '1';
  irw <= irw;
  
  -- Multi-driven assignments
  rcmkmks <= rcmkmks;
  rcmkmks <= rcmkmks;
  rcmkmks <= (others => '0');
  rcmkmks <= (others => '0');
end afugef;



-- Seed after: 8625216856451562431,13788294582928374589
