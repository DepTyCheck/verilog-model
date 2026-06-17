-- Seed: 4902141614519150317,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity fgzpvc is
  port (pcjcmx : out std_logic_vector(2 downto 0); pjkweflqf : out time);
end fgzpvc;

architecture hs of fgzpvc is
  
begin
  -- Single-driven assignments
  pjkweflqf <= 2#0_0_1_1_1.1# ms;
end hs;

library ieee;
use ieee.std_logic_1164.all;

entity buuy is
  port (awf : linkage std_logic; dpaqzsobw : in string(1 to 4));
end buuy;

architecture ix of buuy is
  
begin
  
end ix;

library ieee;
use ieee.std_logic_1164.all;

entity xhiwsuthh is
  port (bdpdifkoa : inout time; pwvcdxrtao : buffer std_logic_vector(4 to 3); xzvxcz : buffer severity_level; ceiqkyqqks : buffer std_logic);
end xhiwsuthh;

library ieee;
use ieee.std_logic_1164.all;

architecture s of xhiwsuthh is
  signal ky : time;
  signal gflxdxaa : std_logic_vector(2 downto 0);
  signal ebohr : string(1 to 4);
  signal vvnqv : std_logic;
  signal jvvggswpj : time;
  signal rap : std_logic_vector(2 downto 0);
begin
  ew : entity work.fgzpvc
    port map (pcjcmx => rap, pjkweflqf => jvvggswpj);
  rl : entity work.fgzpvc
    port map (pcjcmx => rap, pjkweflqf => bdpdifkoa);
  lsz : entity work.buuy
    port map (awf => vvnqv, dpaqzsobw => ebohr);
  gpcywmo : entity work.fgzpvc
    port map (pcjcmx => gflxdxaa, pjkweflqf => ky);
  
  -- Single-driven assignments
  ebohr <= "xscx";
  xzvxcz <= WARNING;
  
  -- Multi-driven assignments
  ceiqkyqqks <= 'X';
  ceiqkyqqks <= 'U';
  ceiqkyqqks <= 'X';
  ceiqkyqqks <= 'Z';
end s;



-- Seed after: 10503315342381541922,10557070023141912087
