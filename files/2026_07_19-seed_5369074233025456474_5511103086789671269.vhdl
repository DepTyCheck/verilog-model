-- Seed: 5369074233025456474,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (lrnhocpo : in std_logic);
end g;

architecture kh of g is
  
begin
  
end kh;

entity gwpkctc is
  port (rpjg : buffer integer; jo : inout real_vector(0 downto 0); tpnmce : buffer real_vector(2 downto 1));
end gwpkctc;

library ieee;
use ieee.std_logic_1164.all;

architecture dxlkhof of gwpkctc is
  signal lq : std_logic;
begin
  pybnbhhdxu : entity work.g
    port map (lrnhocpo => lq);
  
  -- Single-driven assignments
  tpnmce <= (2#10110.010#, 11332.0_0_4_2);
  rpjg <= 0_2;
  jo <= (others => 2#00101.1_1_0_1_1#);
  
  -- Multi-driven assignments
  lq <= 'H';
end dxlkhof;

entity zrftipc is
  port (omgqwwd : buffer integer);
end zrftipc;

library ieee;
use ieee.std_logic_1164.all;

architecture ukz of zrftipc is
  signal zs : std_logic;
  signal sismmy : real_vector(2 downto 1);
  signal xfifn : real_vector(0 downto 0);
  signal f : integer;
  signal kd : real_vector(2 downto 1);
  signal oqrurutxxt : real_vector(0 downto 0);
  signal xxxb : std_logic;
begin
  h : entity work.g
    port map (lrnhocpo => xxxb);
  qirxvtgwa : entity work.gwpkctc
    port map (rpjg => omgqwwd, jo => oqrurutxxt, tpnmce => kd);
  impydhhh : entity work.gwpkctc
    port map (rpjg => f, jo => xfifn, tpnmce => sismmy);
  pqgjq : entity work.g
    port map (lrnhocpo => zs);
  
  -- Multi-driven assignments
  xxxb <= 'Z';
end ukz;

entity lcpk is
  port (eui : inout integer);
end lcpk;

library ieee;
use ieee.std_logic_1164.all;

architecture zybe of lcpk is
  signal k : std_logic;
  signal dljyjjvnbi : std_logic;
begin
  psigaz : entity work.g
    port map (lrnhocpo => dljyjjvnbi);
  p : entity work.g
    port map (lrnhocpo => k);
  nctgmuphsg : entity work.g
    port map (lrnhocpo => dljyjjvnbi);
  
  -- Single-driven assignments
  eui <= 42;
  
  -- Multi-driven assignments
  dljyjjvnbi <= 'Z';
end zybe;



-- Seed after: 3892593843677525634,5511103086789671269
