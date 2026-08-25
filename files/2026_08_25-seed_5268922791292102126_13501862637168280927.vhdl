-- Seed: 5268922791292102126,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity lqaptdikk is
  port (oaucsjkn : inout integer; kdmp : out severity_level; npiot : linkage real; wzjxsf : linkage std_logic);
end lqaptdikk;

architecture inq of lqaptdikk is
  
begin
  -- Single-driven assignments
  kdmp <= kdmp;
  oaucsjkn <= 244;
end inq;

library ieee;
use ieee.std_logic_1164.all;

entity rogdzwx is
  port (xkl : buffer time_vector(3 downto 4); qljxmu : out std_logic);
end rogdzwx;

architecture raujuu of rogdzwx is
  signal fzspvehxq : real;
  signal qur : severity_level;
  signal cidaehi : integer;
  signal nhewpawb : real;
  signal qwzve : severity_level;
  signal ixclgldcq : integer;
  signal kscwspggn : real;
  signal pxdbcn : severity_level;
  signal gms : integer;
begin
  xty : entity work.lqaptdikk
    port map (oaucsjkn => gms, kdmp => pxdbcn, npiot => kscwspggn, wzjxsf => qljxmu);
  lhfogq : entity work.lqaptdikk
    port map (oaucsjkn => ixclgldcq, kdmp => qwzve, npiot => nhewpawb, wzjxsf => qljxmu);
  gjpfqvn : entity work.lqaptdikk
    port map (oaucsjkn => cidaehi, kdmp => qur, npiot => fzspvehxq, wzjxsf => qljxmu);
  
  -- Single-driven assignments
  xkl <= (others => 0 ns);
  
  -- Multi-driven assignments
  qljxmu <= 'H';
  qljxmu <= 'W';
  qljxmu <= 'W';
  qljxmu <= 'U';
end raujuu;



-- Seed after: 7261391703208545594,13501862637168280927
