-- Seed: 17845583654443879969,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity znfnmu is
  port (tag : linkage time_vector(0 to 2); ppgx : inout real_vector(2 downto 1); ahz : linkage integer; f : buffer std_logic);
end znfnmu;

architecture m of znfnmu is
  
begin
  -- Multi-driven assignments
  f <= 'X';
  f <= f;
  f <= f;
end m;

library ieee;
use ieee.std_logic_1164.all;

entity qk is
  port (ul : linkage time; kgymxtr : out real; razfsgzr : linkage std_logic; shg : inout bit);
end qk;

library ieee;
use ieee.std_logic_1164.all;

architecture edghf of qk is
  signal oaiuygov : std_logic;
  signal ayfyxrozeg : integer;
  signal n : real_vector(2 downto 1);
  signal vunzxt : time_vector(0 to 2);
  signal m : std_logic;
  signal qviyh : integer;
  signal qbsjfejjmy : real_vector(2 downto 1);
  signal nujuib : time_vector(0 to 2);
  signal pccckitt : std_logic;
  signal gwo : integer;
  signal bnw : real_vector(2 downto 1);
  signal qrkktzcrgr : time_vector(0 to 2);
begin
  dtoda : entity work.znfnmu
    port map (tag => qrkktzcrgr, ppgx => bnw, ahz => gwo, f => pccckitt);
  afl : entity work.znfnmu
    port map (tag => nujuib, ppgx => qbsjfejjmy, ahz => qviyh, f => m);
  v : entity work.znfnmu
    port map (tag => vunzxt, ppgx => n, ahz => ayfyxrozeg, f => oaiuygov);
  
  -- Single-driven assignments
  shg <= '1';
  kgymxtr <= 8#6_6_0.0655#;
  
  -- Multi-driven assignments
  pccckitt <= pccckitt;
end edghf;

library ieee;
use ieee.std_logic_1164.all;

entity nbzuyqkrfn is
  port (m : buffer std_logic; aqwivumyz : buffer std_logic_vector(2 to 0); ryc : out std_logic_vector(0 to 4));
end nbzuyqkrfn;

library ieee;
use ieee.std_logic_1164.all;

architecture tbvklnzhx of nbzuyqkrfn is
  signal vwraeej : integer;
  signal rvro : real_vector(2 downto 1);
  signal pon : time_vector(0 to 2);
  signal gouvomwyu : std_logic;
  signal bfdz : integer;
  signal w : real_vector(2 downto 1);
  signal ddwupt : time_vector(0 to 2);
begin
  b : entity work.znfnmu
    port map (tag => ddwupt, ppgx => w, ahz => bfdz, f => gouvomwyu);
  ltllnugaz : entity work.znfnmu
    port map (tag => pon, ppgx => rvro, ahz => vwraeej, f => gouvomwyu);
  
  -- Multi-driven assignments
  ryc <= ('W', 'H', 'L', '1', 'L');
  aqwivumyz <= (others => '0');
  ryc <= ryc;
end tbvklnzhx;



-- Seed after: 2932489644732759661,13196211255131729027
