-- Seed: 5294040808139093551,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity hhuyyslb is
  port (xw : in std_logic; pfx : inout string(4 downto 3); lzkdt : in std_logic_vector(2 to 1); bvmuigee : buffer bit_vector(4 downto 0));
end hhuyyslb;

architecture gbj of hhuyyslb is
  
begin
  -- Single-driven assignments
  bvmuigee <= ('1', '0', '0', '1', '0');
  pfx <= ('a', 'u');
end gbj;

library ieee;
use ieee.std_logic_1164.all;

entity loxjlhw is
  port (spuhzyh : linkage severity_level; me : buffer std_logic_vector(3 downto 1); xgpogphubw : out std_logic_vector(0 to 0));
end loxjlhw;

architecture rtw of loxjlhw is
  
begin
  -- Multi-driven assignments
  xgpogphubw <= (others => 'U');
  me <= "0XZ";
  xgpogphubw <= "H";
end rtw;

library ieee;
use ieee.std_logic_1164.all;

entity rrs is
  port (f : inout real; fs : out std_logic_vector(1 downto 3); lqatzmns : linkage severity_level; tplanyxjz : linkage real);
end rrs;

library ieee;
use ieee.std_logic_1164.all;

architecture ykdzzbukf of rrs is
  signal w : bit_vector(4 downto 0);
  signal rrdmxini : std_logic_vector(2 to 1);
  signal nusivsg : string(4 downto 3);
  signal nebfvgunv : bit_vector(4 downto 0);
  signal ffzxyzvyx : string(4 downto 3);
  signal iimqrfcx : std_logic;
  signal wjdi : std_logic_vector(0 to 0);
  signal mskepsymgg : std_logic_vector(3 downto 1);
begin
  qvy : entity work.loxjlhw
    port map (spuhzyh => lqatzmns, me => mskepsymgg, xgpogphubw => wjdi);
  yndxxuk : entity work.hhuyyslb
    port map (xw => iimqrfcx, pfx => ffzxyzvyx, lzkdt => fs, bvmuigee => nebfvgunv);
  nczvnku : entity work.hhuyyslb
    port map (xw => iimqrfcx, pfx => nusivsg, lzkdt => rrdmxini, bvmuigee => w);
  
  -- Single-driven assignments
  f <= 112.0_4_2;
end ykdzzbukf;



-- Seed after: 11323203029918856047,6882842853887419669
