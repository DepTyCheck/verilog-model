-- Seed: 3660947320154177446,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (eqcglhyqcd : linkage std_logic_vector(3 downto 3); anglotgazj : out std_logic; rz : in time; thbshzwxnc : linkage string(1 to 2));
end c;

architecture wz of c is
  
begin
  -- Multi-driven assignments
  anglotgazj <= 'W';
end wz;

library ieee;
use ieee.std_logic_1164.all;

entity aqmkcgyof is
  port (ibwaw : inout std_logic_vector(2 downto 2); vtim : linkage std_logic; vrbazpqgvu : buffer std_logic);
end aqmkcgyof;

library ieee;
use ieee.std_logic_1164.all;

architecture bnj of aqmkcgyof is
  signal rilv : string(1 to 2);
  signal semmcy : std_logic;
  signal ctzpnj : std_logic_vector(3 downto 3);
  signal dipv : string(1 to 2);
  signal kidrlvgyqq : time;
  signal igznhf : std_logic_vector(3 downto 3);
  signal tj : string(1 to 2);
  signal ac : time;
  signal fqnyey : std_logic;
begin
  izyushqkxz : entity work.c
    port map (eqcglhyqcd => ibwaw, anglotgazj => fqnyey, rz => ac, thbshzwxnc => tj);
  awb : entity work.c
    port map (eqcglhyqcd => igznhf, anglotgazj => vrbazpqgvu, rz => kidrlvgyqq, thbshzwxnc => dipv);
  mz : entity work.c
    port map (eqcglhyqcd => ctzpnj, anglotgazj => semmcy, rz => kidrlvgyqq, thbshzwxnc => rilv);
  
  -- Single-driven assignments
  ac <= 4010.0_0_2_3 ns;
  kidrlvgyqq <= 0 sec;
  
  -- Multi-driven assignments
  semmcy <= 'U';
  ctzpnj <= (others => 'Z');
  igznhf <= "U";
end bnj;



-- Seed after: 11382169478394897441,17047277710231705797
