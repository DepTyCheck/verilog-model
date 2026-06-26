-- Seed: 4758529043185040405,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity cqcb is
  port (exrn : linkage real; guytg : in std_logic_vector(2 downto 0); zlursts : in time; axgn : buffer time);
end cqcb;

architecture mc of cqcb is
  
begin
  -- Single-driven assignments
  axgn <= 2#0# ns;
end mc;

entity oqubju is
  port (lnyfngpq : out integer; suynbtqta : buffer time);
end oqubju;

library ieee;
use ieee.std_logic_1164.all;

architecture jbgtpizj of oqubju is
  signal jkihf : std_logic_vector(2 downto 0);
  signal vavht : real;
  signal nqedia : time;
  signal owmo : time;
  signal x : real;
  signal pmy : time;
  signal fnaejz : real;
  signal qryakvvmei : time;
  signal vzot : time;
  signal mgodwozjf : std_logic_vector(2 downto 0);
  signal bhdb : real;
begin
  xqxizx : entity work.cqcb
    port map (exrn => bhdb, guytg => mgodwozjf, zlursts => vzot, axgn => qryakvvmei);
  p : entity work.cqcb
    port map (exrn => fnaejz, guytg => mgodwozjf, zlursts => pmy, axgn => suynbtqta);
  njqdd : entity work.cqcb
    port map (exrn => x, guytg => mgodwozjf, zlursts => owmo, axgn => nqedia);
  gbby : entity work.cqcb
    port map (exrn => vavht, guytg => jkihf, zlursts => qryakvvmei, axgn => vzot);
  
  -- Single-driven assignments
  lnyfngpq <= 8#4#;
  
  -- Multi-driven assignments
  jkihf <= ('W', '1', 'L');
  mgodwozjf <= "1WL";
end jbgtpizj;



-- Seed after: 15312710803620409427,12011142928354116943
