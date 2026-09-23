-- Seed: 4838656303937462572,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity kbyqfvri is
  port (ssbgkwvat : buffer std_logic; ckdxp : inout std_logic_vector(0 to 1); prri : inout std_logic);
end kbyqfvri;

architecture rvmbtqichp of kbyqfvri is
  
begin
  -- Multi-driven assignments
  prri <= 'L';
  prri <= 'W';
  prri <= prri;
  ckdxp <= ('L', 'X');
end rvmbtqichp;

library ieee;
use ieee.std_logic_1164.all;

entity ajr is
  port (iqmfmlqpj : buffer std_logic);
end ajr;

library ieee;
use ieee.std_logic_1164.all;

architecture hzcmck of ajr is
  signal ofgrc : std_logic_vector(0 to 1);
begin
  tcdfp : entity work.kbyqfvri
    port map (ssbgkwvat => iqmfmlqpj, ckdxp => ofgrc, prri => iqmfmlqpj);
  
  -- Multi-driven assignments
  iqmfmlqpj <= '-';
  iqmfmlqpj <= iqmfmlqpj;
  ofgrc <= ('H', 'L');
  iqmfmlqpj <= iqmfmlqpj;
end hzcmck;

library ieee;
use ieee.std_logic_1164.all;

entity wkp is
  port (gwbjkgnm : out severity_level; uksu : inout time; sxktdirtlb : linkage std_logic);
end wkp;

library ieee;
use ieee.std_logic_1164.all;

architecture b of wkp is
  signal ewmojkrrwi : std_logic;
  signal ufpuryic : std_logic_vector(0 to 1);
  signal z : std_logic;
begin
  a : entity work.kbyqfvri
    port map (ssbgkwvat => z, ckdxp => ufpuryic, prri => ewmojkrrwi);
  
  -- Single-driven assignments
  gwbjkgnm <= NOTE;
  uksu <= uksu;
  
  -- Multi-driven assignments
  z <= '-';
  z <= z;
end b;

entity dq is
  port (uoy : in bit);
end dq;

library ieee;
use ieee.std_logic_1164.all;

architecture affocoehvj of dq is
  signal g : std_logic;
  signal fl : std_logic_vector(0 to 1);
  signal saxnca : std_logic;
  signal za : time;
  signal lhranmx : severity_level;
begin
  kpuygaewmc : entity work.wkp
    port map (gwbjkgnm => lhranmx, uksu => za, sxktdirtlb => saxnca);
  yokvjkohw : entity work.kbyqfvri
    port map (ssbgkwvat => saxnca, ckdxp => fl, prri => saxnca);
  ndgxg : entity work.ajr
    port map (iqmfmlqpj => saxnca);
  wmfr : entity work.kbyqfvri
    port map (ssbgkwvat => g, ckdxp => fl, prri => g);
  
  -- Multi-driven assignments
  saxnca <= '-';
  saxnca <= 'H';
end affocoehvj;



-- Seed after: 7279895817899727177,8067602802092121131
