-- Seed: 7539446398454534416,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity pno is
  port (jz : inout std_logic; knifm : inout integer; hgsrv : out real; jmkxlhtym : out real);
end pno;

architecture kbvbitw of pno is
  
begin
  -- Single-driven assignments
  jmkxlhtym <= hgsrv;
  knifm <= 0114;
  hgsrv <= 8#6_0_6.4_2_5#;
  
  -- Multi-driven assignments
  jz <= 'X';
end kbvbitw;

library ieee;
use ieee.std_logic_1164.all;

entity dcw is
  port (vobevima : out integer; ifc : buffer std_logic_vector(4 downto 3); rzgbk : in real; irnrm : linkage time_vector(0 to 0));
end dcw;

library ieee;
use ieee.std_logic_1164.all;

architecture dlataq of dcw is
  signal rc : real;
  signal vrymy : real;
  signal mkbow : std_logic;
  signal rjvbnxeh : real;
  signal nizl : real;
  signal ngru : integer;
  signal ehdtgzrua : std_logic;
begin
  tzlqdsa : entity work.pno
    port map (jz => ehdtgzrua, knifm => ngru, hgsrv => nizl, jmkxlhtym => rjvbnxeh);
  kzlwaav : entity work.pno
    port map (jz => mkbow, knifm => vobevima, hgsrv => vrymy, jmkxlhtym => rc);
  
  -- Multi-driven assignments
  ifc <= ('W', '0');
  ifc <= "W-";
end dlataq;



-- Seed after: 5076717352524837771,10940991575366938685
