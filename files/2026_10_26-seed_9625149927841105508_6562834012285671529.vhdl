-- Seed: 9625149927841105508,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity trgx is
  port (rmbu : buffer integer; zg : in std_logic; sorqlwqqq : in boolean);
end trgx;

architecture hhaynlvm of trgx is
  
begin
  -- Single-driven assignments
  rmbu <= rmbu;
end hhaynlvm;

entity pxkjebw is
  port (osmbk : inout time);
end pxkjebw;

library ieee;
use ieee.std_logic_1164.all;

architecture kkrwyhgpvm of pxkjebw is
  signal wexsn : integer;
  signal xrpoyvlvmn : boolean;
  signal px : std_logic;
  signal duniguro : integer;
begin
  hhsefvt : entity work.trgx
    port map (rmbu => duniguro, zg => px, sorqlwqqq => xrpoyvlvmn);
  ynltgt : entity work.trgx
    port map (rmbu => wexsn, zg => px, sorqlwqqq => xrpoyvlvmn);
  
  -- Single-driven assignments
  osmbk <= osmbk;
  xrpoyvlvmn <= xrpoyvlvmn;
end kkrwyhgpvm;

library ieee;
use ieee.std_logic_1164.all;

entity wca is
  port (hjewtjz : buffer bit; iamkluaa : out std_logic);
end wca;

architecture yobheez of wca is
  
begin
  -- Single-driven assignments
  hjewtjz <= hjewtjz;
  
  -- Multi-driven assignments
  iamkluaa <= iamkluaa;
  iamkluaa <= '0';
  iamkluaa <= '1';
end yobheez;

entity zynqvmaff is
  port (yrl : buffer integer_vector(2 downto 0); yprwke : in integer_vector(2 to 1));
end zynqvmaff;

library ieee;
use ieee.std_logic_1164.all;

architecture ii of zynqvmaff is
  signal srs : bit;
  signal qdybveexq : boolean;
  signal yoncjr : integer;
  signal tsnbsoev : std_logic;
  signal yhmehfouu : integer;
  signal rezrz : boolean;
  signal rwvz : std_logic;
  signal pgxawdnd : integer;
begin
  x : entity work.trgx
    port map (rmbu => pgxawdnd, zg => rwvz, sorqlwqqq => rezrz);
  pubidrlcg : entity work.trgx
    port map (rmbu => yhmehfouu, zg => tsnbsoev, sorqlwqqq => rezrz);
  hekymiow : entity work.trgx
    port map (rmbu => yoncjr, zg => rwvz, sorqlwqqq => qdybveexq);
  pvzjsfm : entity work.wca
    port map (hjewtjz => srs, iamkluaa => rwvz);
  
  -- Multi-driven assignments
  rwvz <= tsnbsoev;
  tsnbsoev <= 'X';
end ii;



-- Seed after: 17250117764260677617,6562834012285671529
