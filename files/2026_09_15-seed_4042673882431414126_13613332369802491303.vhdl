-- Seed: 4042673882431414126,13613332369802491303

entity vhlk is
  port (bou : buffer real);
end vhlk;

architecture itlenunjze of vhlk is
  
begin
  -- Single-driven assignments
  bou <= 8#1.3551#;
end itlenunjze;

entity zjtcnbtd is
  port (pw : buffer time_vector(3 downto 0));
end zjtcnbtd;

architecture vwrqypx of zjtcnbtd is
  signal reetc : real;
begin
  pkypzyz : entity work.vhlk
    port map (bou => reetc);
  
  -- Single-driven assignments
  pw <= pw;
end vwrqypx;

library ieee;
use ieee.std_logic_1164.all;

entity smhv is
  port (vbc : inout real_vector(4 downto 1); nmatdp : inout std_logic);
end smhv;

architecture gpjzbnc of smhv is
  signal nxym : real;
  signal lwukta : real;
  signal lpebi : time_vector(3 downto 0);
begin
  xlfwpls : entity work.zjtcnbtd
    port map (pw => lpebi);
  dzfe : entity work.vhlk
    port map (bou => lwukta);
  fbu : entity work.vhlk
    port map (bou => nxym);
  
  -- Multi-driven assignments
  nmatdp <= 'H';
  nmatdp <= 'U';
end gpjzbnc;

entity oddlqncpcu is
  port (jhdnrhu : out boolean; rzno : out real);
end oddlqncpcu;

architecture a of oddlqncpcu is
  signal p : real;
begin
  wyx : entity work.vhlk
    port map (bou => p);
  
  -- Single-driven assignments
  rzno <= rzno;
  jhdnrhu <= FALSE;
end a;



-- Seed after: 12641039321936060325,13613332369802491303
