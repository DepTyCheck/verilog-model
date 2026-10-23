-- Seed: 14436338369376459537,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (yjncvm : linkage integer; yxmymnzvp : buffer std_logic_vector(0 downto 2); kaoxdpzjg : in integer);
end i;

architecture omdrl of i is
  
begin
  -- Multi-driven assignments
  yxmymnzvp <= "";
  yxmymnzvp <= yxmymnzvp;
end omdrl;

entity nodurb is
  port (hddveri : out integer_vector(2 to 1));
end nodurb;

library ieee;
use ieee.std_logic_1164.all;

architecture bwooyq of nodurb is
  signal zpcvdrlswu : integer;
  signal cvelz : integer;
  signal wzd : std_logic_vector(0 downto 2);
  signal mnkuxrxw : integer;
  signal pnwahrktd : integer;
  signal xmdog : std_logic_vector(0 downto 2);
  signal kxi : integer;
begin
  n : entity work.i
    port map (yjncvm => kxi, yxmymnzvp => xmdog, kaoxdpzjg => pnwahrktd);
  g : entity work.i
    port map (yjncvm => mnkuxrxw, yxmymnzvp => xmdog, kaoxdpzjg => kxi);
  a : entity work.i
    port map (yjncvm => pnwahrktd, yxmymnzvp => wzd, kaoxdpzjg => kxi);
  yi : entity work.i
    port map (yjncvm => cvelz, yxmymnzvp => xmdog, kaoxdpzjg => zpcvdrlswu);
  
  -- Single-driven assignments
  hddveri <= hddveri;
  zpcvdrlswu <= cvelz;
  
  -- Multi-driven assignments
  xmdog <= (others => '0');
  wzd <= (others => '0');
  xmdog <= xmdog;
end bwooyq;

library ieee;
use ieee.std_logic_1164.all;

entity rfjtopo is
  port (obbhlzjs : linkage std_logic; agrhh : linkage string(1 downto 2); rgg : in character);
end rfjtopo;

architecture abczakmyz of rfjtopo is
  
begin
  
end abczakmyz;

library ieee;
use ieee.std_logic_1164.all;

entity tudgvbmlte is
  port (uudhhhvcyj : out std_logic);
end tudgvbmlte;

library ieee;
use ieee.std_logic_1164.all;

architecture wbar of tudgvbmlte is
  signal n : integer;
  signal hrvx : std_logic_vector(0 downto 2);
  signal plant : integer;
begin
  zztzaacaom : entity work.i
    port map (yjncvm => plant, yxmymnzvp => hrvx, kaoxdpzjg => plant);
  mawv : entity work.i
    port map (yjncvm => n, yxmymnzvp => hrvx, kaoxdpzjg => plant);
end wbar;



-- Seed after: 7843212274618705754,6176025545472508329
