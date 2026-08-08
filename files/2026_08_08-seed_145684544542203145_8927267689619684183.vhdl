-- Seed: 145684544542203145,8927267689619684183

entity jtfo is
  port (yqpyqjakre : out time);
end jtfo;

architecture orowzlhra of jtfo is
  
begin
  -- Single-driven assignments
  yqpyqjakre <= yqpyqjakre;
end orowzlhra;

entity m is
  port (onvu : out time; piozgcz : out real; hi : out severity_level);
end m;

architecture msmkfo of m is
  signal qmwg : time;
begin
  dzszlct : entity work.jtfo
    port map (yqpyqjakre => onvu);
  uy : entity work.jtfo
    port map (yqpyqjakre => qmwg);
  
  -- Single-driven assignments
  hi <= FAILURE;
  piozgcz <= piozgcz;
end msmkfo;

library ieee;
use ieee.std_logic_1164.all;

entity nctitff is
  port (tgkvq : inout severity_level; l : buffer time_vector(3 downto 4); jft : inout std_logic_vector(3 to 0));
end nctitff;

architecture sawofniwxy of nctitff is
  signal rda : real;
  signal nvvx : time;
  signal piuqyzaxv : time;
begin
  waza : entity work.jtfo
    port map (yqpyqjakre => piuqyzaxv);
  hvy : entity work.m
    port map (onvu => nvvx, piozgcz => rda, hi => tgkvq);
  
  -- Single-driven assignments
  l <= (others => 0 ns);
end sawofniwxy;



-- Seed after: 13563818855256169064,8927267689619684183
