-- Seed: 6364408181363730039,2338584220606314193

entity banzmfgm is
  port (hwr : buffer bit; oik : out integer_vector(4 downto 2));
end banzmfgm;

architecture scvkwkap of banzmfgm is
  
begin
  -- Single-driven assignments
  oik <= (100, 8#450#, 14240);
end scvkwkap;

library ieee;
use ieee.std_logic_1164.all;

entity rxpkdkf is
  port (voy : inout integer; s : out std_logic; lf : inout std_logic_vector(0 downto 4));
end rxpkdkf;

architecture nukkjumfc of rxpkdkf is
  signal y : integer_vector(4 downto 2);
  signal yu : bit;
begin
  tfnyls : entity work.banzmfgm
    port map (hwr => yu, oik => y);
  
  -- Single-driven assignments
  voy <= voy;
  
  -- Multi-driven assignments
  s <= 'X';
  lf <= "";
end nukkjumfc;

library ieee;
use ieee.std_logic_1164.all;

entity wevnou is
  port (ghzsjt : inout std_logic);
end wevnou;

library ieee;
use ieee.std_logic_1164.all;

architecture s of wevnou is
  signal cjre : std_logic_vector(0 downto 4);
  signal ighowwdbeb : integer;
  signal tgqvgv : integer_vector(4 downto 2);
  signal lkwmmyr : bit;
  signal rqmgkuc : integer_vector(4 downto 2);
  signal afumijg : bit;
  signal ztvxxitdns : integer_vector(4 downto 2);
  signal dbc : bit;
begin
  yaecqjo : entity work.banzmfgm
    port map (hwr => dbc, oik => ztvxxitdns);
  b : entity work.banzmfgm
    port map (hwr => afumijg, oik => rqmgkuc);
  wukf : entity work.banzmfgm
    port map (hwr => lkwmmyr, oik => tgqvgv);
  vhrn : entity work.rxpkdkf
    port map (voy => ighowwdbeb, s => ghzsjt, lf => cjre);
  
  -- Multi-driven assignments
  ghzsjt <= 'H';
  ghzsjt <= 'L';
end s;



-- Seed after: 1880231257433744733,2338584220606314193
