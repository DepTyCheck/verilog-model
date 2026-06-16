-- Seed: 12209886520682436926,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity dockfavqfm is
  port (u : in time_vector(1 to 4); zkz : out integer; dxhdchyzw : inout std_logic_vector(0 to 4));
end dockfavqfm;

architecture jvg of dockfavqfm is
  
begin
  -- Multi-driven assignments
  dxhdchyzw <= ('-', 'U', '1', 'L', 'Z');
  dxhdchyzw <= ('1', 'U', 'L', 'H', '-');
end jvg;

library ieee;
use ieee.std_logic_1164.all;

entity nh is
  port (vebsbqu : buffer std_logic_vector(3 downto 2); tnlwbzb : out integer);
end nh;

library ieee;
use ieee.std_logic_1164.all;

architecture j of nh is
  signal qzgbmdc : std_logic_vector(0 to 4);
  signal dfmpsghyv : std_logic_vector(0 to 4);
  signal ntq : integer;
  signal xikopxxul : time_vector(1 to 4);
  signal zdqwsqb : integer;
  signal vk : time_vector(1 to 4);
  signal epqelk : std_logic_vector(0 to 4);
  signal amk : integer;
  signal frdcwjimgi : time_vector(1 to 4);
begin
  gde : entity work.dockfavqfm
    port map (u => frdcwjimgi, zkz => amk, dxhdchyzw => epqelk);
  lqj : entity work.dockfavqfm
    port map (u => vk, zkz => zdqwsqb, dxhdchyzw => epqelk);
  jcmbivaa : entity work.dockfavqfm
    port map (u => xikopxxul, zkz => ntq, dxhdchyzw => dfmpsghyv);
  wplxxwvm : entity work.dockfavqfm
    port map (u => xikopxxul, zkz => tnlwbzb, dxhdchyzw => qzgbmdc);
  
  -- Single-driven assignments
  frdcwjimgi <= (8#12665.62307# ns, 0 min, 34.31004 ms, 2#0_1_1.1_1_1# fs);
  vk <= (16#5A7.4_5_8_8_6# ns, 2#0_1_0.11111# us, 2 min, 16#86.8# ms);
  xikopxxul <= (8#0_3_0_3.4_1_0# ps, 0 hr, 0_1_3_2 ps, 8#332# fs);
  
  -- Multi-driven assignments
  vebsbqu <= "-0";
  dfmpsghyv <= ('U', 'L', 'H', '1', '1');
  qzgbmdc <= "0ZX10";
end j;

entity iybru is
  port (ecscnvajzb : inout integer);
end iybru;

architecture qklrrpau of iybru is
  
begin
  -- Single-driven assignments
  ecscnvajzb <= 2;
end qklrrpau;



-- Seed after: 13877625608088905688,5472058987609252853
