-- Seed: 13901702838845598582,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity jyduwmaf is
  port (eob : buffer character; wfaqblwonr : in std_logic_vector(2 downto 0));
end jyduwmaf;

architecture rt of jyduwmaf is
  
begin
  -- Single-driven assignments
  eob <= 'a';
end rt;

entity ntrttcf is
  port (acitqsytpx : linkage integer; konv : out severity_level);
end ntrttcf;

library ieee;
use ieee.std_logic_1164.all;

architecture ogkopuiw of ntrttcf is
  signal dlpv : character;
  signal nlffhiw : std_logic_vector(2 downto 0);
  signal b : character;
begin
  oumdnsfh : entity work.jyduwmaf
    port map (eob => b, wfaqblwonr => nlffhiw);
  o : entity work.jyduwmaf
    port map (eob => dlpv, wfaqblwonr => nlffhiw);
  
  -- Single-driven assignments
  konv <= WARNING;
  
  -- Multi-driven assignments
  nlffhiw <= ('H', '-', 'H');
  nlffhiw <= "-WU";
end ogkopuiw;

library ieee;
use ieee.std_logic_1164.all;

entity ztaa is
  port (poahm : out time_vector(0 to 3); qautunzojf : buffer integer; bw : buffer std_logic);
end ztaa;

architecture y of ztaa is
  signal okez : severity_level;
  signal rv : integer;
begin
  mmyrjftsrk : entity work.ntrttcf
    port map (acitqsytpx => rv, konv => okez);
  
  -- Single-driven assignments
  qautunzojf <= 32330;
  poahm <= (4 us, 0_3.4_4_0 ps, 3 sec, 4 sec);
  
  -- Multi-driven assignments
  bw <= 'W';
  bw <= '0';
  bw <= 'X';
  bw <= '1';
end y;



-- Seed after: 10748273878437597724,3924983747739634027
