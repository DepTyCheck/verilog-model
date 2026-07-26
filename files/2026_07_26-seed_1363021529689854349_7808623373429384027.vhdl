-- Seed: 1363021529689854349,7808623373429384027

entity kpsr is
  port (edccovfnw : buffer integer);
end kpsr;

architecture x of kpsr is
  
begin
  -- Single-driven assignments
  edccovfnw <= 3;
end x;

library ieee;
use ieee.std_logic_1164.all;

entity nsnt is
  port (qbqzetaj : out real; okimqyu : linkage std_logic; txlstzmie : inout real);
end nsnt;

architecture jfcdydob of nsnt is
  signal qsoj : integer;
  signal ipxfzzcmj : integer;
begin
  xfzxpbux : entity work.kpsr
    port map (edccovfnw => ipxfzzcmj);
  xhosuo : entity work.kpsr
    port map (edccovfnw => qsoj);
  
  -- Single-driven assignments
  txlstzmie <= txlstzmie;
end jfcdydob;

entity qwa is
  port (yb : out real);
end qwa;

library ieee;
use ieee.std_logic_1164.all;

architecture v of qwa is
  signal dzxpprbfj : real;
  signal wjlcuw : std_logic;
  signal xdpeih : integer;
  signal itdhk : integer;
  signal tdn : integer;
begin
  f : entity work.kpsr
    port map (edccovfnw => tdn);
  n : entity work.kpsr
    port map (edccovfnw => itdhk);
  zmtpbcgs : entity work.kpsr
    port map (edccovfnw => xdpeih);
  bwgglfz : entity work.nsnt
    port map (qbqzetaj => yb, okimqyu => wjlcuw, txlstzmie => dzxpprbfj);
end v;



-- Seed after: 17178223272946552898,7808623373429384027
