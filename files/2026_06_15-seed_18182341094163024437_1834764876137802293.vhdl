-- Seed: 18182341094163024437,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (c : linkage time; bdokr : inout bit_vector(2 to 0); wyrzhauhm : inout real; xxluqlm : inout std_logic);
end qe;

architecture r of qe is
  
begin
  -- Single-driven assignments
  wyrzhauhm <= 2#0_1.11110#;
  bdokr <= (others => '0');
  
  -- Multi-driven assignments
  xxluqlm <= 'W';
  xxluqlm <= '-';
end r;

entity c is
  port (xrjyn : in boolean; aimpbj : in bit_vector(3 downto 1); m : linkage boolean);
end c;

architecture odd of c is
  
begin
  
end odd;

entity pwthw is
  port (vvnh : linkage integer; mt : buffer time);
end pwthw;

library ieee;
use ieee.std_logic_1164.all;

architecture yn of pwthw is
  signal mexlhgqku : std_logic;
  signal u : real;
  signal ekwa : bit_vector(2 to 0);
  signal vw : time;
  signal w : boolean;
  signal vms : bit_vector(3 downto 1);
  signal iakzrw : boolean;
  signal ioupihvxpr : std_logic;
  signal zkyvq : real;
  signal lypexyxshz : bit_vector(2 to 0);
  signal oazkp : std_logic;
  signal vhhjypvxg : real;
  signal l : bit_vector(2 to 0);
  signal fwnj : time;
begin
  gxxqpc : entity work.qe
    port map (c => fwnj, bdokr => l, wyrzhauhm => vhhjypvxg, xxluqlm => oazkp);
  vfdoop : entity work.qe
    port map (c => mt, bdokr => lypexyxshz, wyrzhauhm => zkyvq, xxluqlm => ioupihvxpr);
  dnr : entity work.c
    port map (xrjyn => iakzrw, aimpbj => vms, m => w);
  yblz : entity work.qe
    port map (c => vw, bdokr => ekwa, wyrzhauhm => u, xxluqlm => mexlhgqku);
  
  -- Single-driven assignments
  iakzrw <= TRUE;
  vms <= ('0', '0', '1');
end yn;

library ieee;
use ieee.std_logic_1164.all;

entity yflmnvy is
  port (irlablfoha : buffer std_logic_vector(2 downto 1); l : linkage std_logic);
end yflmnvy;

library ieee;
use ieee.std_logic_1164.all;

architecture srixk of yflmnvy is
  signal v : std_logic;
  signal lamhap : real;
  signal ztyeqq : bit_vector(2 to 0);
  signal cnqeqi : time;
  signal dkamj : boolean;
  signal axv : bit_vector(3 downto 1);
  signal fssohkja : boolean;
begin
  smasocgt : entity work.c
    port map (xrjyn => fssohkja, aimpbj => axv, m => dkamj);
  rfln : entity work.qe
    port map (c => cnqeqi, bdokr => ztyeqq, wyrzhauhm => lamhap, xxluqlm => v);
  pqu : entity work.c
    port map (xrjyn => fssohkja, aimpbj => axv, m => fssohkja);
  
  -- Single-driven assignments
  axv <= ('1', '0', '1');
end srixk;



-- Seed after: 9732220937868594337,1834764876137802293
