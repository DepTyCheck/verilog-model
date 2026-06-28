-- Seed: 16546194363109598834,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (vnhibsl : buffer boolean_vector(0 to 4); gszpb : inout std_logic; crbpz : inout boolean);
end p;

architecture d of p is
  
begin
  -- Multi-driven assignments
  gszpb <= 'W';
  gszpb <= 'H';
  gszpb <= '0';
end d;

library ieee;
use ieee.std_logic_1164.all;

entity jolofgo is
  port (mrcl : inout bit_vector(1 downto 4); xcrgiz : inout integer; k : buffer character; he : inout std_logic_vector(3 downto 1));
end jolofgo;

architecture ixn of jolofgo is
  
begin
  -- Single-driven assignments
  k <= 'g';
  xcrgiz <= 41;
  mrcl <= (others => '0');
end ixn;

entity x is
  port (rvcuxb : inout time; xk : out real);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture jhbck of x is
  signal gcqcwybhxb : boolean;
  signal pcuwymzled : std_logic;
  signal zx : boolean_vector(0 to 4);
  signal kodw : std_logic_vector(3 downto 1);
  signal qxcqih : character;
  signal pbxlntexik : integer;
  signal lb : bit_vector(1 downto 4);
begin
  na : entity work.jolofgo
    port map (mrcl => lb, xcrgiz => pbxlntexik, k => qxcqih, he => kodw);
  vc : entity work.p
    port map (vnhibsl => zx, gszpb => pcuwymzled, crbpz => gcqcwybhxb);
  
  -- Single-driven assignments
  xk <= 3_1_4_0.2141;
  rvcuxb <= 41 ps;
  
  -- Multi-driven assignments
  pcuwymzled <= 'X';
  pcuwymzled <= '0';
  kodw <= "1HH";
end jhbck;



-- Seed after: 8693690329373336016,6697892553037813751
