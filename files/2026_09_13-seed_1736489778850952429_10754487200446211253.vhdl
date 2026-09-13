-- Seed: 1736489778850952429,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity thnslouhap is
  port (zwtrwcwjj : linkage std_logic; ilwqe : in time; nankjdvlwo : buffer time_vector(2 downto 1));
end thnslouhap;

architecture orw of thnslouhap is
  
begin
  -- Single-driven assignments
  nankjdvlwo <= (2#011# ms, 2#1_1_0_0.1# fs);
end orw;

entity rjvlkv is
  port (vmhw : buffer boolean; mfttkw : linkage time);
end rjvlkv;

library ieee;
use ieee.std_logic_1164.all;

architecture t of rjvlkv is
  signal ytddt : time_vector(2 downto 1);
  signal goq : time;
  signal jrcmzmncbf : std_logic;
  signal bpuvfj : time_vector(2 downto 1);
  signal pcllukhv : time;
  signal qwxikxiy : std_logic;
  signal yriqokozl : time_vector(2 downto 1);
  signal eijz : time;
  signal oi : time_vector(2 downto 1);
  signal fqkcstnlfg : time;
  signal ldcga : std_logic;
begin
  xy : entity work.thnslouhap
    port map (zwtrwcwjj => ldcga, ilwqe => fqkcstnlfg, nankjdvlwo => oi);
  yclmpikbpn : entity work.thnslouhap
    port map (zwtrwcwjj => ldcga, ilwqe => eijz, nankjdvlwo => yriqokozl);
  am : entity work.thnslouhap
    port map (zwtrwcwjj => qwxikxiy, ilwqe => pcllukhv, nankjdvlwo => bpuvfj);
  avtbaj : entity work.thnslouhap
    port map (zwtrwcwjj => jrcmzmncbf, ilwqe => goq, nankjdvlwo => ytddt);
  
  -- Single-driven assignments
  goq <= 2.3_1_4_2_1 ps;
  vmhw <= TRUE;
  eijz <= 8#1.3_3_5_0# ms;
  fqkcstnlfg <= 8#44453# fs;
  
  -- Multi-driven assignments
  ldcga <= ldcga;
  ldcga <= ldcga;
  jrcmzmncbf <= 'H';
end t;



-- Seed after: 4565877956561821831,10754487200446211253
