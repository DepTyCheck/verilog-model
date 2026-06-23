-- Seed: 3428343112384303913,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity rl is
  port (cckpjyghm : in time_vector(4 to 2); jmvrokzzf : inout std_logic; tt : inout boolean; vjxnvtyqxt : buffer real);
end rl;

architecture wldebhczs of rl is
  
begin
  -- Single-driven assignments
  vjxnvtyqxt <= 13314.4_2_0;
  tt <= TRUE;
  
  -- Multi-driven assignments
  jmvrokzzf <= 'U';
  jmvrokzzf <= 'X';
  jmvrokzzf <= 'H';
  jmvrokzzf <= '0';
end wldebhczs;

entity gpgghzdgu is
  port (uhkwnyi : inout time);
end gpgghzdgu;

library ieee;
use ieee.std_logic_1164.all;

architecture v of gpgghzdgu is
  signal doya : real;
  signal l : boolean;
  signal e : std_logic;
  signal r : time_vector(4 to 2);
begin
  ov : entity work.rl
    port map (cckpjyghm => r, jmvrokzzf => e, tt => l, vjxnvtyqxt => doya);
  
  -- Single-driven assignments
  uhkwnyi <= 2_2_1_1_0.2_3_1_3_2 ns;
  r <= (others => 0 ns);
  
  -- Multi-driven assignments
  e <= 'U';
  e <= '-';
  e <= 'Z';
end v;



-- Seed after: 8958708411519515304,8421704836678237495
