-- Seed: 14973996426980014655,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity ou is
  port (mldi : buffer std_logic_vector(3 downto 1); bdaqfnfjg : inout real; tyocga : linkage integer; gxezdg : in boolean_vector(2 downto 1));
end ou;

architecture vzwcxbsyu of ou is
  
begin
  -- Multi-driven assignments
  mldi <= ('W', 'H', 'W');
end vzwcxbsyu;

entity we is
  port (pk : linkage time);
end we;

library ieee;
use ieee.std_logic_1164.all;

architecture bgf of we is
  signal fgtp : boolean_vector(2 downto 1);
  signal jfm : integer;
  signal oor : real;
  signal oysceiqod : std_logic_vector(3 downto 1);
begin
  uzwcqcpd : entity work.ou
    port map (mldi => oysceiqod, bdaqfnfjg => oor, tyocga => jfm, gxezdg => fgtp);
  
  -- Single-driven assignments
  fgtp <= (FALSE, FALSE);
  
  -- Multi-driven assignments
  oysceiqod <= "WUZ";
end bgf;

library ieee;
use ieee.std_logic_1164.all;

entity je is
  port (pat : linkage time; kib : out std_logic_vector(0 downto 0); eytdjnt : linkage std_logic_vector(1 to 0));
end je;

library ieee;
use ieee.std_logic_1164.all;

architecture ebucc of je is
  signal jv : integer;
  signal qkrwlvg : real;
  signal xexmzwmem : boolean_vector(2 downto 1);
  signal jy : integer;
  signal vtggscp : real;
  signal hrtswg : std_logic_vector(3 downto 1);
begin
  gcenruezh : entity work.ou
    port map (mldi => hrtswg, bdaqfnfjg => vtggscp, tyocga => jy, gxezdg => xexmzwmem);
  bpszd : entity work.ou
    port map (mldi => hrtswg, bdaqfnfjg => qkrwlvg, tyocga => jv, gxezdg => xexmzwmem);
  laron : entity work.we
    port map (pk => pat);
  
  -- Single-driven assignments
  xexmzwmem <= (TRUE, TRUE);
  
  -- Multi-driven assignments
  kib <= (others => 'W');
  kib <= "H";
end ebucc;



-- Seed after: 9066439944915052149,14629254427735353553
