-- Seed: 10497911691192634032,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity zoc is
  port (fxvozjhrt : out std_logic; laamxxl : buffer boolean; ahir : linkage std_logic_vector(2 downto 3); cwdpv : out std_logic);
end zoc;

architecture nwate of zoc is
  
begin
  -- Single-driven assignments
  laamxxl <= FALSE;
end nwate;

entity cghd is
  port (cywsfuwonh : out integer; gpv : in time);
end cghd;

library ieee;
use ieee.std_logic_1164.all;

architecture j of cghd is
  signal qkydsabf : std_logic;
  signal ycxvee : std_logic_vector(2 downto 3);
  signal shozubsni : boolean;
  signal mrdrhmij : std_logic;
begin
  ctnejtjkgl : entity work.zoc
    port map (fxvozjhrt => mrdrhmij, laamxxl => shozubsni, ahir => ycxvee, cwdpv => qkydsabf);
  
  -- Single-driven assignments
  cywsfuwonh <= 4;
  
  -- Multi-driven assignments
  mrdrhmij <= 'U';
end j;

library ieee;
use ieee.std_logic_1164.all;

entity ny is
  port (qbqktvgphh : buffer integer; p : in std_logic; uocp : linkage std_logic);
end ny;

library ieee;
use ieee.std_logic_1164.all;

architecture zcr of ny is
  signal prplaekgx : std_logic;
  signal xxdavwd : boolean;
  signal gkcnk : time;
  signal xasejzmbn : integer;
  signal nzxnxxowp : std_logic;
  signal l : std_logic_vector(2 downto 3);
  signal veuggnffbt : boolean;
  signal lldwqqi : std_logic;
begin
  hstmvqint : entity work.zoc
    port map (fxvozjhrt => lldwqqi, laamxxl => veuggnffbt, ahir => l, cwdpv => nzxnxxowp);
  wbho : entity work.cghd
    port map (cywsfuwonh => xasejzmbn, gpv => gkcnk);
  mulppb : entity work.zoc
    port map (fxvozjhrt => lldwqqi, laamxxl => xxdavwd, ahir => l, cwdpv => prplaekgx);
  
  -- Multi-driven assignments
  prplaekgx <= '1';
  lldwqqi <= '1';
  lldwqqi <= '-';
  lldwqqi <= 'X';
end zcr;



-- Seed after: 18079050190002373213,8421704836678237495
