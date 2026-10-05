-- Seed: 13603427963216565494,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (sebqmvyule : inout time_vector(3 to 1); zwjfkxnyyr : out std_logic);
end r;

architecture zak of r is
  
begin
  -- Single-driven assignments
  sebqmvyule <= (others => 0 ns);
  
  -- Multi-driven assignments
  zwjfkxnyyr <= 'U';
end zak;

library ieee;
use ieee.std_logic_1164.all;

entity wtcnibsjm is
  port (ul : buffer std_logic_vector(4 to 0));
end wtcnibsjm;

library ieee;
use ieee.std_logic_1164.all;

architecture ogxyi of wtcnibsjm is
  signal mgmp : std_logic;
  signal vm : time_vector(3 to 1);
begin
  zumw : entity work.r
    port map (sebqmvyule => vm, zwjfkxnyyr => mgmp);
  
  -- Multi-driven assignments
  ul <= (others => '0');
  ul <= "";
end ogxyi;



-- Seed after: 3107753797746105411,7304262412290825129
