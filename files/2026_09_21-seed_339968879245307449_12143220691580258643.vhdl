-- Seed: 339968879245307449,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity pwby is
  port (ugbvxm : out integer; kqentgewv : buffer std_logic; d : buffer std_logic_vector(0 to 3));
end pwby;

architecture x of pwby is
  
begin
  -- Single-driven assignments
  ugbvxm <= ugbvxm;
  
  -- Multi-driven assignments
  d <= ('W', 'Z', '-', '0');
  d <= ('0', 'L', '1', 'Z');
  d <= d;
end x;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (wxopmk : buffer real_vector(4 downto 2); tsnxvzrq : buffer real; cc : buffer std_logic);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture cxo of d is
  signal liftmphyv : std_logic;
  signal zgxts : integer;
  signal qmnfuslzb : integer;
  signal y : std_logic;
  signal bxg : integer;
  signal plr : std_logic_vector(0 to 3);
  signal wwwwksfsdl : integer;
begin
  waxrjgpm : entity work.pwby
    port map (ugbvxm => wwwwksfsdl, kqentgewv => cc, d => plr);
  ku : entity work.pwby
    port map (ugbvxm => bxg, kqentgewv => y, d => plr);
  sq : entity work.pwby
    port map (ugbvxm => qmnfuslzb, kqentgewv => y, d => plr);
  ztuuisiqs : entity work.pwby
    port map (ugbvxm => zgxts, kqentgewv => liftmphyv, d => plr);
  
  -- Single-driven assignments
  tsnxvzrq <= tsnxvzrq;
  wxopmk <= (3_2.43, 2_1_1.1, 2.1);
  
  -- Multi-driven assignments
  y <= cc;
  cc <= cc;
  y <= 'Z';
  liftmphyv <= 'L';
end cxo;

library ieee;
use ieee.std_logic_1164.all;

entity psfwrbisu is
  port (iwzugoscd : in std_logic);
end psfwrbisu;

architecture nxier of psfwrbisu is
  
begin
  
end nxier;

entity xcd is
  port (chtladicn : in integer; sdmiseropa : inout integer_vector(2 downto 1));
end xcd;

library ieee;
use ieee.std_logic_1164.all;

architecture ugyqy of xcd is
  signal cukibcn : std_logic;
  signal bffvotv : real;
  signal wmsbmoyba : real_vector(4 downto 2);
  signal qajz : std_logic_vector(0 to 3);
  signal pbog : integer;
  signal fvnjojjuzi : std_logic_vector(0 to 3);
  signal u : integer;
  signal rmt : std_logic;
begin
  ztyacdesfb : entity work.psfwrbisu
    port map (iwzugoscd => rmt);
  plbvj : entity work.pwby
    port map (ugbvxm => u, kqentgewv => rmt, d => fvnjojjuzi);
  b : entity work.pwby
    port map (ugbvxm => pbog, kqentgewv => rmt, d => qajz);
  lymmyx : entity work.d
    port map (wxopmk => wmsbmoyba, tsnxvzrq => bffvotv, cc => cukibcn);
  
  -- Single-driven assignments
  sdmiseropa <= sdmiseropa;
  
  -- Multi-driven assignments
  rmt <= '1';
  cukibcn <= '0';
end ugyqy;



-- Seed after: 677794941705495816,12143220691580258643
