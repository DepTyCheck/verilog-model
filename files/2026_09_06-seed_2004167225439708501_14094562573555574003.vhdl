-- Seed: 2004167225439708501,14094562573555574003

entity trahmqikeg is
  port (wbvmnyr : in time; ffk : in real);
end trahmqikeg;

architecture igsssjh of trahmqikeg is
  
begin
  
end igsssjh;

entity x is
  port (a : buffer integer_vector(1 downto 0); ghfsakukml : in bit; mufddfsa : out real);
end x;

architecture zedl of x is
  signal acnq : real;
  signal emwbppztws : time;
begin
  fzg : entity work.trahmqikeg
    port map (wbvmnyr => emwbppztws, ffk => acnq);
  
  -- Single-driven assignments
  mufddfsa <= 3_2_4_1_4.2_0;
  acnq <= acnq;
end zedl;

library ieee;
use ieee.std_logic_1164.all;

entity oxjuqhkii is
  port (g : buffer std_logic);
end oxjuqhkii;

architecture bpikwmn of oxjuqhkii is
  signal leepz : real;
  signal zyb : time;
  signal gyekyhi : time;
  signal kfgmpygsu : real;
  signal zc : bit;
  signal uc : integer_vector(1 downto 0);
begin
  ppgkvxie : entity work.x
    port map (a => uc, ghfsakukml => zc, mufddfsa => kfgmpygsu);
  ts : entity work.trahmqikeg
    port map (wbvmnyr => gyekyhi, ffk => kfgmpygsu);
  zxv : entity work.trahmqikeg
    port map (wbvmnyr => zyb, ffk => leepz);
  
  -- Single-driven assignments
  zyb <= gyekyhi;
  
  -- Multi-driven assignments
  g <= g;
  g <= 'W';
end bpikwmn;



-- Seed after: 4267136284125118067,14094562573555574003
