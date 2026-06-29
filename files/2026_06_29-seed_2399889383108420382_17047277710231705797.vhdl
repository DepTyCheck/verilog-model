-- Seed: 2399889383108420382,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity ch is
  port (arlwmz : buffer std_logic_vector(4 downto 3); rydanrsjw : out std_logic);
end ch;

architecture u of ch is
  
begin
  -- Multi-driven assignments
  rydanrsjw <= 'Z';
end u;

library ieee;
use ieee.std_logic_1164.all;

entity wkljswijd is
  port (fljfouedtk : out std_logic_vector(3 to 4); epirsr : in std_logic);
end wkljswijd;

library ieee;
use ieee.std_logic_1164.all;

architecture afrerz of wkljswijd is
  signal npvb : std_logic;
  signal pkszdedttj : std_logic;
  signal pbscvk : std_logic_vector(4 downto 3);
  signal lnchmwt : std_logic;
  signal yuylj : std_logic_vector(4 downto 3);
  signal qvqhar : std_logic;
  signal mhfq : std_logic_vector(4 downto 3);
begin
  nt : entity work.ch
    port map (arlwmz => mhfq, rydanrsjw => qvqhar);
  jstt : entity work.ch
    port map (arlwmz => yuylj, rydanrsjw => lnchmwt);
  yaqvh : entity work.ch
    port map (arlwmz => pbscvk, rydanrsjw => pkszdedttj);
  xqm : entity work.ch
    port map (arlwmz => fljfouedtk, rydanrsjw => npvb);
  
  -- Multi-driven assignments
  pbscvk <= ('X', '1');
  fljfouedtk <= ('U', '0');
  pbscvk <= "ZH";
end afrerz;

library ieee;
use ieee.std_logic_1164.all;

entity ntkhlasy is
  port (lroh : inout time; y : inout std_logic_vector(0 downto 4); fygb : out integer; bqmsl : inout severity_level);
end ntkhlasy;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ntkhlasy is
  signal ojxc : std_logic;
  signal akz : std_logic_vector(3 to 4);
begin
  sycxrv : entity work.ch
    port map (arlwmz => akz, rydanrsjw => ojxc);
  z : entity work.wkljswijd
    port map (fljfouedtk => akz, epirsr => ojxc);
  
  -- Single-driven assignments
  bqmsl <= NOTE;
  lroh <= 30304 ms;
  fygb <= 16#F_B_C#;
end i;

entity nc is
  port (mmsctzvm : buffer bit_vector(0 downto 2));
end nc;

architecture zvrpjgmtsn of nc is
  
begin
  -- Single-driven assignments
  mmsctzvm <= (others => '0');
end zvrpjgmtsn;



-- Seed after: 5694344782865774888,17047277710231705797
