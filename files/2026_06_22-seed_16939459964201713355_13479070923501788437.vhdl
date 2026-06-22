-- Seed: 16939459964201713355,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity tjnyfpwt is
  port (rrzeodx : buffer std_logic; uxmic : out real_vector(1 to 1); zbfb : linkage integer);
end tjnyfpwt;

architecture xooluiahxx of tjnyfpwt is
  
begin
  -- Multi-driven assignments
  rrzeodx <= '-';
  rrzeodx <= 'H';
  rrzeodx <= '0';
end xooluiahxx;

library ieee;
use ieee.std_logic_1164.all;

entity ost is
  port (zlnooabc : out std_logic; tnseueyqxb : out character);
end ost;

architecture ic of ost is
  
begin
  -- Single-driven assignments
  tnseueyqxb <= 'y';
  
  -- Multi-driven assignments
  zlnooabc <= 'H';
  zlnooabc <= '0';
  zlnooabc <= 'H';
  zlnooabc <= '-';
end ic;

entity afnhsdn is
  port (stjrlym : buffer character; poynwla : out boolean; zsklpluaoa : out boolean);
end afnhsdn;

library ieee;
use ieee.std_logic_1164.all;

architecture zzttps of afnhsdn is
  signal g : integer;
  signal ersxoxk : real_vector(1 to 1);
  signal meylhn : std_logic;
  signal sixdmqzl : integer;
  signal djoqlcjf : real_vector(1 to 1);
  signal nopvcrrojp : std_logic;
  signal isveju : integer;
  signal lfuqjmkjal : real_vector(1 to 1);
  signal fvljq : std_logic;
begin
  xom : entity work.ost
    port map (zlnooabc => fvljq, tnseueyqxb => stjrlym);
  qaz : entity work.tjnyfpwt
    port map (rrzeodx => fvljq, uxmic => lfuqjmkjal, zbfb => isveju);
  vl : entity work.tjnyfpwt
    port map (rrzeodx => nopvcrrojp, uxmic => djoqlcjf, zbfb => sixdmqzl);
  xfxzgys : entity work.tjnyfpwt
    port map (rrzeodx => meylhn, uxmic => ersxoxk, zbfb => g);
  
  -- Single-driven assignments
  zsklpluaoa <= TRUE;
  poynwla <= TRUE;
  
  -- Multi-driven assignments
  nopvcrrojp <= 'H';
end zzttps;



-- Seed after: 316912619948007766,13479070923501788437
