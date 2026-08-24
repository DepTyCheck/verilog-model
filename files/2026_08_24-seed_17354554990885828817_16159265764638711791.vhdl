-- Seed: 17354554990885828817,16159265764638711791

entity qlmmunwln is
  port (vvzjnc : linkage integer; cweyvqp : inout real_vector(2 downto 1));
end qlmmunwln;

architecture jnviovwzk of qlmmunwln is
  
begin
  -- Single-driven assignments
  cweyvqp <= cweyvqp;
end jnviovwzk;

entity gchonjccj is
  port (t : linkage integer; qribqhtu : linkage time; heabwysjz : out real; kckxltux : out time);
end gchonjccj;

architecture bnwxvnatc of gchonjccj is
  signal b : real_vector(2 downto 1);
  signal hmdlptmjx : real_vector(2 downto 1);
  signal s : integer;
  signal br : real_vector(2 downto 1);
  signal nzde : integer;
begin
  axmllzgkfm : entity work.qlmmunwln
    port map (vvzjnc => nzde, cweyvqp => br);
  rv : entity work.qlmmunwln
    port map (vvzjnc => s, cweyvqp => hmdlptmjx);
  uvuobcaqr : entity work.qlmmunwln
    port map (vvzjnc => t, cweyvqp => b);
end bnwxvnatc;

library ieee;
use ieee.std_logic_1164.all;

entity qxpc is
  port (cgihpbw : inout std_logic_vector(4 to 0); pahlpxt : buffer time);
end qxpc;

architecture hm of qxpc is
  signal fhvkzjs : real_vector(2 downto 1);
  signal ennxdwktlo : integer;
begin
  fvvyh : entity work.qlmmunwln
    port map (vvzjnc => ennxdwktlo, cweyvqp => fhvkzjs);
  
  -- Single-driven assignments
  pahlpxt <= 4 sec;
  
  -- Multi-driven assignments
  cgihpbw <= "";
end hm;



-- Seed after: 14661868110756625819,16159265764638711791
