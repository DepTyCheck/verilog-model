-- Seed: 10650351134735746320,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity ohruldhyl is
  port (burp : inout std_logic_vector(0 to 1); p : out real; tv : out bit; o : inout std_logic_vector(2 to 0));
end ohruldhyl;

architecture kyblk of ohruldhyl is
  
begin
  -- Single-driven assignments
  tv <= tv;
  p <= 2#10111.0_1_1_0#;
  
  -- Multi-driven assignments
  burp <= burp;
  o <= "";
end kyblk;



-- Seed after: 2163611513783304934,7304262412290825129
