-- Seed: 8924787056061222286,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity pxbehweym is
  port (yasuvosyp : out real; sv : buffer std_logic_vector(2 to 2));
end pxbehweym;

architecture yvjkljq of pxbehweym is
  
begin
  -- Single-driven assignments
  yasuvosyp <= 02.1_4_4;
  
  -- Multi-driven assignments
  sv <= "U";
  sv <= "W";
  sv <= "H";
  sv <= (others => 'U');
end yvjkljq;

entity uifwftey is
  port (hw : in time; edosk : linkage integer);
end uifwftey;

library ieee;
use ieee.std_logic_1164.all;

architecture dggbkco of uifwftey is
  signal lpdunlalv : std_logic_vector(2 to 2);
  signal poi : real;
begin
  cidkwjf : entity work.pxbehweym
    port map (yasuvosyp => poi, sv => lpdunlalv);
end dggbkco;

library ieee;
use ieee.std_logic_1164.all;

entity jjuntohkzg is
  port (gg : buffer string(1 downto 3); rfkx : linkage std_logic; m : out time_vector(2 to 0));
end jjuntohkzg;

library ieee;
use ieee.std_logic_1164.all;

architecture itxsyqnz of jjuntohkzg is
  signal ocxcj : real;
  signal vcfkzmjqx : std_logic_vector(2 to 2);
  signal osgfeaiwov : real;
begin
  dw : entity work.pxbehweym
    port map (yasuvosyp => osgfeaiwov, sv => vcfkzmjqx);
  ywahrtdy : entity work.pxbehweym
    port map (yasuvosyp => ocxcj, sv => vcfkzmjqx);
  
  -- Single-driven assignments
  m <= (others => 0 ns);
  gg <= (others => ' ');
  
  -- Multi-driven assignments
  vcfkzmjqx <= "X";
  vcfkzmjqx <= (others => '-');
  vcfkzmjqx <= (others => 'W');
end itxsyqnz;



-- Seed after: 1765253606533311205,10557070023141912087
