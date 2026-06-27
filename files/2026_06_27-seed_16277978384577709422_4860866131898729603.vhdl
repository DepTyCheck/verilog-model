-- Seed: 16277978384577709422,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity xioelxnuig is
  port (ynxvj : buffer std_logic);
end xioelxnuig;

architecture u of xioelxnuig is
  
begin
  -- Multi-driven assignments
  ynxvj <= 'U';
  ynxvj <= 'U';
  ynxvj <= '0';
  ynxvj <= 'W';
end u;

entity job is
  port (ryxuqmbg : out integer_vector(4 to 4); fvpz : out string(3 to 1));
end job;

library ieee;
use ieee.std_logic_1164.all;

architecture awgwhdssz of job is
  signal jbrgpg : std_logic;
  signal w : std_logic;
begin
  jds : entity work.xioelxnuig
    port map (ynxvj => w);
  napmjgm : entity work.xioelxnuig
    port map (ynxvj => w);
  dpgf : entity work.xioelxnuig
    port map (ynxvj => jbrgpg);
  
  -- Single-driven assignments
  fvpz <= (others => ' ');
  ryxuqmbg <= (others => 2#10111#);
  
  -- Multi-driven assignments
  jbrgpg <= 'U';
end awgwhdssz;



-- Seed after: 11042243102921835182,4860866131898729603
