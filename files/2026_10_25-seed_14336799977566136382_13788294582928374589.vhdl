-- Seed: 14336799977566136382,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity zmparuoqu is
  port (wdoyy : buffer std_logic; hublndgelq : buffer integer; zxfjqwiju : in time_vector(1 to 3); ry : linkage time);
end zmparuoqu;

architecture iurgkewgy of zmparuoqu is
  
begin
  -- Single-driven assignments
  hublndgelq <= 3434;
end iurgkewgy;

library ieee;
use ieee.std_logic_1164.all;

entity ph is
  port (ozaixsxvm : linkage std_logic);
end ph;

library ieee;
use ieee.std_logic_1164.all;

architecture xkmgzynv of ph is
  signal qz : time;
  signal aqspmiglw : time_vector(1 to 3);
  signal ddamjo : integer;
  signal ccvqvdjr : std_logic;
  signal lpj : time;
  signal jinsexk : integer;
  signal ylrob : std_logic;
  signal bqhzmwbvl : time;
  signal que : time_vector(1 to 3);
  signal sahbrvmrm : integer;
  signal riunqshf : std_logic;
begin
  dip : entity work.zmparuoqu
    port map (wdoyy => riunqshf, hublndgelq => sahbrvmrm, zxfjqwiju => que, ry => bqhzmwbvl);
  uishovmai : entity work.zmparuoqu
    port map (wdoyy => ylrob, hublndgelq => jinsexk, zxfjqwiju => que, ry => lpj);
  mburjfl : entity work.zmparuoqu
    port map (wdoyy => ccvqvdjr, hublndgelq => ddamjo, zxfjqwiju => aqspmiglw, ry => qz);
  
  -- Single-driven assignments
  que <= (2 min, 2#01000# us, 2412 ms);
  aqspmiglw <= (16#768# fs, 2 sec, 1_0_2_3 ns);
  
  -- Multi-driven assignments
  riunqshf <= riunqshf;
  riunqshf <= 'W';
  riunqshf <= 'L';
  ccvqvdjr <= riunqshf;
end xkmgzynv;

entity x is
  port (aljowvlb : in real);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture dzmcqd of x is
  signal lwbttupzq : time;
  signal nrl : time_vector(1 to 3);
  signal tmagwdff : integer;
  signal uyrj : std_logic;
begin
  vtmdamgfe : entity work.zmparuoqu
    port map (wdoyy => uyrj, hublndgelq => tmagwdff, zxfjqwiju => nrl, ry => lwbttupzq);
  
  -- Single-driven assignments
  nrl <= (4 min, 8#1_6_4# us, 2#1111# ns);
  
  -- Multi-driven assignments
  uyrj <= 'L';
  uyrj <= '1';
end dzmcqd;

entity iaisumx is
  port (j : linkage severity_level; ikz : linkage real; hpd : in integer);
end iaisumx;

library ieee;
use ieee.std_logic_1164.all;

architecture uk of iaisumx is
  signal jhclduqo : time;
  signal qvp : time_vector(1 to 3);
  signal izqmwddsxu : integer;
  signal jxmx : std_logic;
  signal ang : std_logic;
  signal ps : std_logic;
begin
  g : entity work.ph
    port map (ozaixsxvm => ps);
  u : entity work.ph
    port map (ozaixsxvm => ang);
  npoi : entity work.zmparuoqu
    port map (wdoyy => jxmx, hublndgelq => izqmwddsxu, zxfjqwiju => qvp, ry => jhclduqo);
  
  -- Multi-driven assignments
  ps <= ps;
  ang <= ps;
end uk;



-- Seed after: 9572061031747840483,13788294582928374589
