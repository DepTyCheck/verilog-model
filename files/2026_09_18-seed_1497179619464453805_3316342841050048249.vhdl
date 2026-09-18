-- Seed: 1497179619464453805,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity koabuptwd is
  port (efmhg : inout time; ooekegrsa : in time_vector(3 to 4); qgk : inout std_logic_vector(3 to 2));
end koabuptwd;

architecture blv of koabuptwd is
  
begin
  -- Single-driven assignments
  efmhg <= efmhg;
  
  -- Multi-driven assignments
  qgk <= "";
  qgk <= (others => '0');
  qgk <= qgk;
  qgk <= (others => '0');
end blv;

library ieee;
use ieee.std_logic_1164.all;

entity ot is
  port (mmzerobif : buffer bit; vnefck : inout real; hnfcpbruc : in std_logic);
end ot;

architecture refdno of ot is
  
begin
  -- Single-driven assignments
  mmzerobif <= mmzerobif;
end refdno;

entity seavj is
  port (mexsmgn : in time; mzwvadgu : out character);
end seavj;

library ieee;
use ieee.std_logic_1164.all;

architecture krf of seavj is
  signal rgclfz : std_logic;
  signal ede : real;
  signal xtql : bit;
  signal znkxieqa : std_logic_vector(3 to 2);
  signal nbd : time_vector(3 to 4);
  signal iokftn : time;
begin
  jhvld : entity work.koabuptwd
    port map (efmhg => iokftn, ooekegrsa => nbd, qgk => znkxieqa);
  bzldhrq : entity work.ot
    port map (mmzerobif => xtql, vnefck => ede, hnfcpbruc => rgclfz);
  
  -- Single-driven assignments
  mzwvadgu <= 'b';
  nbd <= nbd;
end krf;

entity uamkjiydmc is
  port (qcwlbzsv : out time; gcmb : out integer; ezycuj : out real);
end uamkjiydmc;

library ieee;
use ieee.std_logic_1164.all;

architecture uatmjbqr of uamkjiydmc is
  signal vlyvunbs : std_logic_vector(3 to 2);
  signal yzjpsbqs : time_vector(3 to 4);
  signal ww : real;
  signal rbmgyzsj : bit;
  signal zcl : std_logic;
  signal xrskfsr : bit;
begin
  grz : entity work.ot
    port map (mmzerobif => xrskfsr, vnefck => ezycuj, hnfcpbruc => zcl);
  rtsljx : entity work.ot
    port map (mmzerobif => rbmgyzsj, vnefck => ww, hnfcpbruc => zcl);
  kpwjet : entity work.koabuptwd
    port map (efmhg => qcwlbzsv, ooekegrsa => yzjpsbqs, qgk => vlyvunbs);
  
  -- Single-driven assignments
  gcmb <= 1_0_1_0_3;
  yzjpsbqs <= (1 hr, 40 ms);
  
  -- Multi-driven assignments
  zcl <= zcl;
  vlyvunbs <= "";
  zcl <= zcl;
end uatmjbqr;



-- Seed after: 207017397832885969,3316342841050048249
