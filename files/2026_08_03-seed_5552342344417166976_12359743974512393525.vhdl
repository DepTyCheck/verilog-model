-- Seed: 5552342344417166976,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (mzm : out std_logic);
end f;

architecture aj of f is
  
begin
  -- Multi-driven assignments
  mzm <= mzm;
  mzm <= mzm;
  mzm <= mzm;
end aj;

library ieee;
use ieee.std_logic_1164.all;

entity fgn is
  port (lexmimp : inout integer; gwjxutu : buffer std_logic);
end fgn;

library ieee;
use ieee.std_logic_1164.all;

architecture kdhnhty of fgn is
  signal muuay : std_logic;
  signal mgwlqbehb : std_logic;
begin
  i : entity work.f
    port map (mzm => mgwlqbehb);
  hoyr : entity work.f
    port map (mzm => gwjxutu);
  xvvaajpoa : entity work.f
    port map (mzm => muuay);
  
  -- Single-driven assignments
  lexmimp <= lexmimp;
  
  -- Multi-driven assignments
  mgwlqbehb <= gwjxutu;
  gwjxutu <= '0';
  muuay <= 'X';
end kdhnhty;

entity kgdprcs is
  port (layktvk : linkage bit; tramzg : out time; djpmeohtkx : inout boolean);
end kgdprcs;

library ieee;
use ieee.std_logic_1164.all;

architecture lwhc of kgdprcs is
  signal ywth : std_logic;
  signal whlb : std_logic;
begin
  wt : entity work.f
    port map (mzm => whlb);
  cpzwyfrwqd : entity work.f
    port map (mzm => ywth);
end lwhc;



-- Seed after: 4819735374275323141,12359743974512393525
