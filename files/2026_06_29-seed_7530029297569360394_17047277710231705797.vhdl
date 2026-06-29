-- Seed: 7530029297569360394,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity dncrdaff is
  port (hqjpnl : linkage std_logic_vector(4 to 3); yhbmnvnj : out integer; hthn : linkage std_logic_vector(2 downto 4); au : out real);
end dncrdaff;

architecture ygvvhho of dncrdaff is
  
begin
  -- Single-driven assignments
  au <= 2043.333;
end ygvvhho;

entity azoeup is
  port (ultzra : inout time; b : linkage real; acdq : out time; as : linkage real_vector(3 downto 2));
end azoeup;

library ieee;
use ieee.std_logic_1164.all;

architecture qh of azoeup is
  signal jpmx : real;
  signal iktfbyu : integer;
  signal gf : real;
  signal qpcspwczu : std_logic_vector(2 downto 4);
  signal gav : integer;
  signal vf : std_logic_vector(2 downto 4);
begin
  iquewdu : entity work.dncrdaff
    port map (hqjpnl => vf, yhbmnvnj => gav, hthn => qpcspwczu, au => gf);
  zlbsoy : entity work.dncrdaff
    port map (hqjpnl => vf, yhbmnvnj => iktfbyu, hthn => vf, au => jpmx);
  
  -- Single-driven assignments
  acdq <= 3 min;
  ultzra <= 1 hr;
  
  -- Multi-driven assignments
  vf <= (others => '0');
end qh;



-- Seed after: 8368103472445765310,17047277710231705797
