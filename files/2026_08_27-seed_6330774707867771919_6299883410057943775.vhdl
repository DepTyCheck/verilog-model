-- Seed: 6330774707867771919,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity ivnw is
  port (sqvpq : buffer std_logic_vector(2 downto 1); ix : linkage integer; rpll : out std_logic; nvmyhlnv : buffer time);
end ivnw;

architecture izathb of ivnw is
  
begin
  -- Single-driven assignments
  nvmyhlnv <= 16#E.27F# fs;
  
  -- Multi-driven assignments
  sqvpq <= ('W', 'W');
end izathb;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (vwkzoi : out std_logic_vector(3 to 1); zhtwgs : inout boolean; lhpiiscydv : buffer integer; edmwe : inout time_vector(3 to 3));
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture tomdez of x is
  signal nwasdquxn : time;
  signal piuvvasz : std_logic;
  signal jfg : integer;
  signal ost : time;
  signal zakecge : std_logic;
  signal cbz : integer;
  signal zakq : std_logic_vector(2 downto 1);
begin
  p : entity work.ivnw
    port map (sqvpq => zakq, ix => cbz, rpll => zakecge, nvmyhlnv => ost);
  ek : entity work.ivnw
    port map (sqvpq => zakq, ix => jfg, rpll => piuvvasz, nvmyhlnv => nwasdquxn);
  
  -- Single-driven assignments
  lhpiiscydv <= 16#6B7#;
  zhtwgs <= TRUE;
  edmwe <= (others => 1 sec);
  
  -- Multi-driven assignments
  zakq <= zakq;
end tomdez;



-- Seed after: 8591276983002696087,6299883410057943775
