-- Seed: 1277987339977480471,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity vplmrxe is
  port (bqdam : in time_vector(4 to 3); zfviyf : linkage time_vector(3 downto 2); w : buffer std_logic; gshhi : inout integer);
end vplmrxe;

architecture qsexvmweh of vplmrxe is
  
begin
  -- Single-driven assignments
  gshhi <= 314;
  
  -- Multi-driven assignments
  w <= 'Z';
  w <= '-';
  w <= 'H';
end qsexvmweh;

entity bt is
  port (b : out integer; mklzzervtk : in real; pxooimmt : linkage time; cpgp : out severity_level);
end bt;

library ieee;
use ieee.std_logic_1164.all;

architecture obauz of bt is
  signal vbzeigs : integer;
  signal qwxkubkc : std_logic;
  signal nsvzofr : time_vector(3 downto 2);
  signal ldolbnrsj : time_vector(4 to 3);
begin
  vtgqnsk : entity work.vplmrxe
    port map (bqdam => ldolbnrsj, zfviyf => nsvzofr, w => qwxkubkc, gshhi => vbzeigs);
  
  -- Single-driven assignments
  cpgp <= WARNING;
  ldolbnrsj <= (others => 0 ns);
  b <= 8#11427#;
  
  -- Multi-driven assignments
  qwxkubkc <= 'Z';
  qwxkubkc <= 'X';
  qwxkubkc <= '-';
  qwxkubkc <= '0';
end obauz;



-- Seed after: 558291954514786232,6882842853887419669
