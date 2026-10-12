-- Seed: 1314108064593415768,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity fiukyj is
  port (libnmgsz : inout std_logic; hfbluzh : out time; fxbwpdh : buffer std_logic);
end fiukyj;

architecture hzqzosvntj of fiukyj is
  
begin
  -- Single-driven assignments
  hfbluzh <= hfbluzh;
  
  -- Multi-driven assignments
  fxbwpdh <= fxbwpdh;
end hzqzosvntj;

entity lpejwre is
  port (boc : out integer_vector(0 to 4));
end lpejwre;

library ieee;
use ieee.std_logic_1164.all;

architecture cvn of lpejwre is
  signal tg : std_logic;
  signal ruatwf : time;
  signal dsnobp : std_logic;
  signal l : time;
  signal lfsagqmn : std_logic;
begin
  k : entity work.fiukyj
    port map (libnmgsz => lfsagqmn, hfbluzh => l, fxbwpdh => lfsagqmn);
  hsirug : entity work.fiukyj
    port map (libnmgsz => dsnobp, hfbluzh => ruatwf, fxbwpdh => tg);
  
  -- Multi-driven assignments
  dsnobp <= 'Z';
  lfsagqmn <= 'H';
  dsnobp <= 'X';
  lfsagqmn <= 'W';
end cvn;



-- Seed after: 9993490623543197497,3426943171337919827
