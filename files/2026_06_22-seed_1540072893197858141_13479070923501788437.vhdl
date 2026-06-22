-- Seed: 1540072893197858141,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity kmdmux is
  port (yy : buffer std_logic_vector(1 downto 2); xke : in std_logic_vector(3 to 0); yxspo : in std_logic_vector(4 to 3); g : inout time);
end kmdmux;

architecture rr of kmdmux is
  
begin
  -- Single-driven assignments
  g <= 1 sec;
  
  -- Multi-driven assignments
  yy <= (others => '0');
  yy <= (others => '0');
  yy <= "";
  yy <= (others => '0');
end rr;

entity ey is
  port (abkkqvh : in bit_vector(0 to 4));
end ey;

library ieee;
use ieee.std_logic_1164.all;

architecture bxg of ey is
  signal l : time;
  signal pzr : std_logic_vector(4 to 3);
  signal hanvt : std_logic_vector(3 to 0);
  signal fhsdd : time;
  signal ywl : std_logic_vector(3 to 0);
  signal qmixypkm : std_logic_vector(1 downto 2);
begin
  j : entity work.kmdmux
    port map (yy => qmixypkm, xke => ywl, yxspo => qmixypkm, g => fhsdd);
  pelhlx : entity work.kmdmux
    port map (yy => qmixypkm, xke => hanvt, yxspo => pzr, g => l);
  
  -- Multi-driven assignments
  ywl <= (others => '0');
end bxg;



-- Seed after: 2086591082795499518,13479070923501788437
