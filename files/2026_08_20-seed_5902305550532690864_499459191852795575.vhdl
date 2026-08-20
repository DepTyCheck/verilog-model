-- Seed: 5902305550532690864,499459191852795575

entity uicqydhvuz is
  port (swmshw : inout time; lydbqxtltu : out real);
end uicqydhvuz;

architecture ralnaxqbuv of uicqydhvuz is
  
begin
  
end ralnaxqbuv;

library ieee;
use ieee.std_logic_1164.all;

entity kaerxal is
  port (shsigrg : in real; z : inout std_logic_vector(2 to 0); lk : out real; aizjoc : inout time);
end kaerxal;

architecture heqk of kaerxal is
  signal pqi : time;
  signal zjawiebu : real;
begin
  wh : entity work.uicqydhvuz
    port map (swmshw => aizjoc, lydbqxtltu => zjawiebu);
  fpsxqziaq : entity work.uicqydhvuz
    port map (swmshw => pqi, lydbqxtltu => lk);
  
  -- Multi-driven assignments
  z <= z;
end heqk;

entity jwemazkdf is
  port (f : inout time; tcvfuij : linkage integer);
end jwemazkdf;

library ieee;
use ieee.std_logic_1164.all;

architecture kwdssdorfp of jwemazkdf is
  signal a : std_logic_vector(2 to 0);
  signal rqdhtxbv : real;
  signal lahqfkz : real;
  signal gavqd : time;
  signal cn : time;
  signal flmjzt : real;
  signal lrobkt : std_logic_vector(2 to 0);
  signal sq : time;
  signal vcvshfl : real;
  signal arvset : std_logic_vector(2 to 0);
  signal yk : real;
begin
  xbsu : entity work.kaerxal
    port map (shsigrg => yk, z => arvset, lk => vcvshfl, aizjoc => sq);
  xmbhhzczak : entity work.kaerxal
    port map (shsigrg => yk, z => lrobkt, lk => flmjzt, aizjoc => cn);
  m : entity work.uicqydhvuz
    port map (swmshw => gavqd, lydbqxtltu => lahqfkz);
  z : entity work.kaerxal
    port map (shsigrg => rqdhtxbv, z => a, lk => yk, aizjoc => f);
  
  -- Single-driven assignments
  rqdhtxbv <= 20111.31;
  
  -- Multi-driven assignments
  arvset <= (others => '0');
end kwdssdorfp;



-- Seed after: 9985416052774298401,499459191852795575
