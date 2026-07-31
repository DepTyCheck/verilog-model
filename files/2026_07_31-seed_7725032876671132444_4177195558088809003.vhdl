-- Seed: 7725032876671132444,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity wgpp is
  port (q : buffer std_logic; wwbhbwp : out std_logic);
end wgpp;

architecture xzcutb of wgpp is
  
begin
  -- Multi-driven assignments
  q <= wwbhbwp;
  wwbhbwp <= '1';
  q <= wwbhbwp;
  wwbhbwp <= wwbhbwp;
end xzcutb;

library ieee;
use ieee.std_logic_1164.all;

entity kfy is
  port (yjg : linkage real; lrsfgxosla : out time; falzphg : out std_logic; j : out real);
end kfy;

library ieee;
use ieee.std_logic_1164.all;

architecture auta of kfy is
  signal ywass : std_logic;
  signal ivgjvbz : std_logic;
  signal rkqoecw : std_logic;
begin
  ip : entity work.wgpp
    port map (q => rkqoecw, wwbhbwp => falzphg);
  dengcdlyp : entity work.wgpp
    port map (q => ivgjvbz, wwbhbwp => ywass);
  
  -- Single-driven assignments
  j <= 16#73DA.B477#;
  lrsfgxosla <= lrsfgxosla;
  
  -- Multi-driven assignments
  falzphg <= falzphg;
  falzphg <= falzphg;
  falzphg <= ywass;
end auta;



-- Seed after: 2904309559191038295,4177195558088809003
