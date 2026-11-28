-- Seed: 7306230687865289068,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity kzqziu is
  port (xrto : inout time; tf : in std_logic_vector(2 to 2); uznaj : buffer bit; fx : linkage std_logic);
end kzqziu;

architecture rsgosqaqt of kzqziu is
  
begin
  
end rsgosqaqt;

library ieee;
use ieee.std_logic_1164.all;

entity vyjmbwcdev is
  port (s : in std_logic; lqfbdu : in integer; bs : linkage std_logic_vector(3 downto 2));
end vyjmbwcdev;

library ieee;
use ieee.std_logic_1164.all;

architecture m of vyjmbwcdev is
  signal vaycpictsq : bit;
  signal q : time;
  signal pciwk : bit;
  signal vkbqg : time;
  signal e : std_logic;
  signal icgaoh : bit;
  signal dkdwydxam : time;
  signal r : bit;
  signal iwxgden : std_logic_vector(2 to 2);
  signal axbsxqlal : time;
begin
  gcjl : entity work.kzqziu
    port map (xrto => axbsxqlal, tf => iwxgden, uznaj => r, fx => s);
  nvj : entity work.kzqziu
    port map (xrto => dkdwydxam, tf => iwxgden, uznaj => icgaoh, fx => e);
  flukzjdg : entity work.kzqziu
    port map (xrto => vkbqg, tf => iwxgden, uznaj => pciwk, fx => e);
  rgm : entity work.kzqziu
    port map (xrto => q, tf => iwxgden, uznaj => vaycpictsq, fx => s);
  
  -- Multi-driven assignments
  iwxgden <= (others => '0');
end m;

library ieee;
use ieee.std_logic_1164.all;

entity bk is
  port (q : out time; ocaxyzzod : inout time_vector(3 downto 0); bmzaex : buffer integer; dw : out std_logic);
end bk;

library ieee;
use ieee.std_logic_1164.all;

architecture pszgbca of bk is
  signal gntxapg : std_logic_vector(3 downto 2);
  signal cea : integer;
begin
  cr : entity work.vyjmbwcdev
    port map (s => dw, lqfbdu => cea, bs => gntxapg);
  
  -- Single-driven assignments
  cea <= bmzaex;
  q <= 8#6_2_7# fs;
  ocaxyzzod <= (16#F94A.6# us, 2#0_0.1_1# ns, 0_2_4_3.001 fs, 4.2 us);
  bmzaex <= 2#1#;
  
  -- Multi-driven assignments
  dw <= 'W';
  gntxapg <= gntxapg;
  dw <= 'X';
end pszgbca;



-- Seed after: 7436593444426549019,15306146742274979323
