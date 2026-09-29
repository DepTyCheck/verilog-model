-- Seed: 13617144382359380436,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity aasmf is
  port (kqbs : out real; kqrnuq : buffer time; s : in std_logic_vector(3 downto 4); gtiw : inout std_logic_vector(0 to 1));
end aasmf;

architecture hhfvrngqt of aasmf is
  
begin
  -- Single-driven assignments
  kqbs <= kqbs;
  kqrnuq <= 1 hr;
  
  -- Multi-driven assignments
  gtiw <= ('L', 'L');
  gtiw <= "-L";
end hhfvrngqt;

entity ztk is
  port (sm : out string(4 downto 1); omoqe : out time; irv : linkage severity_level; wbihpasd : buffer character);
end ztk;

library ieee;
use ieee.std_logic_1164.all;

architecture twh of ztk is
  signal zz : std_logic_vector(0 to 1);
  signal zmto : real;
  signal ote : time;
  signal wb : real;
  signal usncne : std_logic_vector(0 to 1);
  signal refjpm : std_logic_vector(3 downto 4);
  signal zyoual : time;
  signal qdvvapmj : real;
begin
  ewdxs : entity work.aasmf
    port map (kqbs => qdvvapmj, kqrnuq => zyoual, s => refjpm, gtiw => usncne);
  zadu : entity work.aasmf
    port map (kqbs => wb, kqrnuq => ote, s => refjpm, gtiw => usncne);
  s : entity work.aasmf
    port map (kqbs => zmto, kqrnuq => omoqe, s => refjpm, gtiw => zz);
  
  -- Single-driven assignments
  wbihpasd <= wbihpasd;
  sm <= sm;
  
  -- Multi-driven assignments
  refjpm <= refjpm;
  zz <= usncne;
  refjpm <= "";
end twh;



-- Seed after: 5390770872153117886,10940991575366938685
