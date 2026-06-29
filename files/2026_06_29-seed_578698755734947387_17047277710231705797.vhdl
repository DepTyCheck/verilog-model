-- Seed: 578698755734947387,17047277710231705797

entity lbldmy is
  port (ege : in integer; jnumvtb : linkage time; igbntb : inout boolean);
end lbldmy;

architecture rdvn of lbldmy is
  
begin
  -- Single-driven assignments
  igbntb <= FALSE;
end rdvn;

library ieee;
use ieee.std_logic_1164.all;

entity wo is
  port (y : out std_logic; a : buffer std_logic; zckdydgcs : buffer std_logic_vector(0 downto 2); txzcwa : buffer std_logic);
end wo;

architecture wlorqnydwb of wo is
  signal mpctnoewd : boolean;
  signal gnte : time;
  signal gjo : integer;
begin
  irelgsckkx : entity work.lbldmy
    port map (ege => gjo, jnumvtb => gnte, igbntb => mpctnoewd);
  
  -- Single-driven assignments
  gjo <= 8#1_7_2_6#;
  
  -- Multi-driven assignments
  zckdydgcs <= "";
end wlorqnydwb;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (xuj : buffer integer; migv : out character; ugezhsoya : buffer std_logic_vector(2 to 0); dwzkh : linkage bit);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture xrujpcscdt of w is
  signal phfnbgtyd : boolean;
  signal xotfpq : time;
  signal ije : boolean;
  signal zlnwag : time;
  signal rpi : boolean;
  signal vd : time;
  signal pkqnvcr : integer;
  signal wrerh : std_logic_vector(0 downto 2);
  signal nfgjwud : std_logic;
  signal xhndqsj : std_logic;
begin
  afz : entity work.wo
    port map (y => xhndqsj, a => nfgjwud, zckdydgcs => wrerh, txzcwa => nfgjwud);
  lkslx : entity work.lbldmy
    port map (ege => pkqnvcr, jnumvtb => vd, igbntb => rpi);
  mkjmhys : entity work.lbldmy
    port map (ege => pkqnvcr, jnumvtb => zlnwag, igbntb => ije);
  wsukantwt : entity work.lbldmy
    port map (ege => xuj, jnumvtb => xotfpq, igbntb => phfnbgtyd);
  
  -- Single-driven assignments
  migv <= 't';
  pkqnvcr <= 4333;
  xuj <= 8#5_3_6#;
  
  -- Multi-driven assignments
  xhndqsj <= 'W';
  wrerh <= "";
  ugezhsoya <= "";
  ugezhsoya <= "";
end xrujpcscdt;

library ieee;
use ieee.std_logic_1164.all;

entity cl is
  port (xc : linkage character; sgjdp : buffer std_logic_vector(4 downto 4); ltsatpgi : linkage real; clvvxhtnuu : inout integer);
end cl;

library ieee;
use ieee.std_logic_1164.all;

architecture wkt of cl is
  signal byhwwbylxq : bit;
  signal kceuruhexj : std_logic_vector(2 to 0);
  signal cveqk : character;
  signal ss : boolean;
  signal m : time;
begin
  xinzslt : entity work.lbldmy
    port map (ege => clvvxhtnuu, jnumvtb => m, igbntb => ss);
  fsowraofky : entity work.w
    port map (xuj => clvvxhtnuu, migv => cveqk, ugezhsoya => kceuruhexj, dwzkh => byhwwbylxq);
  
  -- Multi-driven assignments
  sgjdp <= "Z";
end wkt;



-- Seed after: 14617185489987115269,17047277710231705797
