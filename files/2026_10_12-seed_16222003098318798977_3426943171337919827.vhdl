-- Seed: 16222003098318798977,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity zxrapiku is
  port (utdlkbtcpw : inout std_logic_vector(3 to 3));
end zxrapiku;

architecture ewgwukheb of zxrapiku is
  
begin
  -- Multi-driven assignments
  utdlkbtcpw <= "U";
  utdlkbtcpw <= utdlkbtcpw;
  utdlkbtcpw <= (others => 'X');
end ewgwukheb;

entity hrcj is
  port (fc : in time);
end hrcj;

library ieee;
use ieee.std_logic_1164.all;

architecture ydmyj of hrcj is
  signal v : std_logic_vector(3 to 3);
begin
  h : entity work.zxrapiku
    port map (utdlkbtcpw => v);
  zyqa : entity work.zxrapiku
    port map (utdlkbtcpw => v);
  lqgmcfc : entity work.zxrapiku
    port map (utdlkbtcpw => v);
  
  -- Multi-driven assignments
  v <= (others => '0');
  v <= v;
end ydmyj;



-- Seed after: 16379210171477121209,3426943171337919827
