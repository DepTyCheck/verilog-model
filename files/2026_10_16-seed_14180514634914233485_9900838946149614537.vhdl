-- Seed: 14180514634914233485,9900838946149614537

entity sw is
  port (eccatzfo : in bit_vector(0 to 4));
end sw;

architecture cg of sw is
  
begin
  
end cg;

library ieee;
use ieee.std_logic_1164.all;

entity wupm is
  port ( skidzbtdxk : linkage std_logic_vector(3 downto 3)
  ; aawvzgpkw : buffer std_logic
  ; ts : out boolean_vector(0 downto 0)
  ; nshgfjf : buffer std_logic_vector(3 downto 2)
  );
end wupm;

architecture y of wupm is
  signal a : bit_vector(0 to 4);
begin
  f : entity work.sw
    port map (eccatzfo => a);
  sndip : entity work.sw
    port map (eccatzfo => a);
  
  -- Single-driven assignments
  ts <= (others => TRUE);
  a <= a;
  
  -- Multi-driven assignments
  aawvzgpkw <= aawvzgpkw;
end y;



-- Seed after: 2511703008670182100,9900838946149614537
