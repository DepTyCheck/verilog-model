-- Seed: 3973347633245372369,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity wmh is
  port (velr : out boolean; pyolkdr : inout std_logic_vector(4 to 2); q : linkage severity_level; ivj : in integer);
end wmh;

architecture bgczm of wmh is
  
begin
  -- Single-driven assignments
  velr <= TRUE;
  
  -- Multi-driven assignments
  pyolkdr <= (others => '0');
  pyolkdr <= "";
end bgczm;

entity rwotlqk is
  port (zxd : buffer string(3 downto 4));
end rwotlqk;

library ieee;
use ieee.std_logic_1164.all;

architecture rwbhfub of rwotlqk is
  signal onpcw : severity_level;
  signal qzun : std_logic_vector(4 to 2);
  signal togxt : boolean;
  signal yh : severity_level;
  signal qfibin : std_logic_vector(4 to 2);
  signal m : boolean;
  signal scc : severity_level;
  signal kkakh : boolean;
  signal jks : integer;
  signal qj : severity_level;
  signal arefv : std_logic_vector(4 to 2);
  signal toc : boolean;
begin
  jbs : entity work.wmh
    port map (velr => toc, pyolkdr => arefv, q => qj, ivj => jks);
  sz : entity work.wmh
    port map (velr => kkakh, pyolkdr => arefv, q => scc, ivj => jks);
  zgsqow : entity work.wmh
    port map (velr => m, pyolkdr => qfibin, q => yh, ivj => jks);
  zcnjk : entity work.wmh
    port map (velr => togxt, pyolkdr => qzun, q => onpcw, ivj => jks);
  
  -- Single-driven assignments
  zxd <= "";
  
  -- Multi-driven assignments
  qzun <= arefv;
  qfibin <= arefv;
end rwbhfub;

entity edzfbwsdh is
  port (kgrdktw : out integer; w : buffer severity_level);
end edzfbwsdh;

architecture vm of edzfbwsdh is
  
begin
  -- Single-driven assignments
  kgrdktw <= 4_0_2_1_1;
  w <= WARNING;
end vm;



-- Seed after: 7089097201888404762,11481034001933599325
