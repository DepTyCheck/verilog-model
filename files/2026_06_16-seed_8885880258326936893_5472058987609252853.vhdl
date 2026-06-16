-- Seed: 8885880258326936893,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity iby is
  port (ahbhoqgqz : out std_logic);
end iby;

architecture zxrs of iby is
  
begin
  -- Multi-driven assignments
  ahbhoqgqz <= 'X';
  ahbhoqgqz <= '-';
end zxrs;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (asv : out time; n : buffer std_logic_vector(3 downto 2); wr : buffer time; jntbqw : in real);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture wmpdto of a is
  signal rrdaswk : std_logic;
  signal errpua : std_logic;
begin
  l : entity work.iby
    port map (ahbhoqgqz => errpua);
  ypemymg : entity work.iby
    port map (ahbhoqgqz => rrdaswk);
  eifk : entity work.iby
    port map (ahbhoqgqz => errpua);
  
  -- Single-driven assignments
  wr <= 8#2_4_4# ns;
  asv <= 1_0_4_1.4_4_1 us;
  
  -- Multi-driven assignments
  n <= "HX";
  n <= ('L', 'X');
  errpua <= 'W';
end wmpdto;



-- Seed after: 2675118800068961212,5472058987609252853
