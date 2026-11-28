-- Seed: 12989392527883385469,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (smmnw : out std_logic_vector(2 downto 3); jskyhduq : out time; dks : out std_logic_vector(0 downto 0));
end z;

architecture dgyhmulyxw of z is
  
begin
  -- Single-driven assignments
  jskyhduq <= jskyhduq;
  
  -- Multi-driven assignments
  dks <= (others => 'L');
end dgyhmulyxw;

library ieee;
use ieee.std_logic_1164.all;

entity qzhedftf is
  port (g : linkage std_logic_vector(2 downto 1); qqeuhhhio : inout boolean_vector(1 to 3));
end qzhedftf;

library ieee;
use ieee.std_logic_1164.all;

architecture djg of qzhedftf is
  signal spfgq : std_logic_vector(0 downto 0);
  signal zkxt : time;
  signal q : std_logic_vector(0 downto 0);
  signal txsffd : time;
  signal xw : std_logic_vector(2 downto 3);
begin
  o : entity work.z
    port map (smmnw => xw, jskyhduq => txsffd, dks => q);
  sslgyqbf : entity work.z
    port map (smmnw => xw, jskyhduq => zkxt, dks => spfgq);
  
  -- Multi-driven assignments
  xw <= (others => '0');
  q <= q;
end djg;



-- Seed after: 8609154157993389591,15306146742274979323
