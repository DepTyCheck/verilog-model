-- Seed: 8634542838527002868,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity tubibiq is
  port (dve : out std_logic; cbohz : out real);
end tubibiq;

architecture tf of tubibiq is
  
begin
  -- Single-driven assignments
  cbohz <= 16#5.D_B#;
  
  -- Multi-driven assignments
  dve <= 'Z';
  dve <= 'X';
  dve <= '0';
end tf;

library ieee;
use ieee.std_logic_1164.all;

entity jfjfpcmh is
  port (d : out integer; prizifvjrb : out integer; mmxqqg : buffer integer_vector(3 to 4); njzuaxbmqc : in std_logic);
end jfjfpcmh;

library ieee;
use ieee.std_logic_1164.all;

architecture uupikbvt of jfjfpcmh is
  signal xwrphz : real;
  signal dlaglhykq : std_logic;
begin
  zbgpfdcck : entity work.tubibiq
    port map (dve => dlaglhykq, cbohz => xwrphz);
  
  -- Single-driven assignments
  prizifvjrb <= 1021;
  
  -- Multi-driven assignments
  dlaglhykq <= 'Z';
  dlaglhykq <= '-';
  dlaglhykq <= 'U';
end uupikbvt;

entity zlplipfu is
  port (xjiqdakyp : out real; xsrhq : out time);
end zlplipfu;

library ieee;
use ieee.std_logic_1164.all;

architecture sxdcku of zlplipfu is
  signal xvctsq : real;
  signal fvbyr : std_logic;
begin
  hdra : entity work.tubibiq
    port map (dve => fvbyr, cbohz => xvctsq);
  
  -- Single-driven assignments
  xsrhq <= 4_1_3 ps;
  xjiqdakyp <= 1_2.4;
  
  -- Multi-driven assignments
  fvbyr <= 'W';
end sxdcku;



-- Seed after: 5458203067154823759,13479070923501788437
