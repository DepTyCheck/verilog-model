-- Seed: 11448887827772983202,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity flkif is
  port (eytuxkuni : inout real; ddunt : buffer std_logic; imhvlx : buffer std_logic);
end flkif;

architecture hqusz of flkif is
  
begin
  -- Single-driven assignments
  eytuxkuni <= 2#0_0.0_1_1_0#;
  
  -- Multi-driven assignments
  imhvlx <= 'L';
end hqusz;

entity jqgk is
  port (t : inout time_vector(0 downto 3); peiev : buffer integer);
end jqgk;

library ieee;
use ieee.std_logic_1164.all;

architecture zhnyiwbqkf of jqgk is
  signal ujgvopoe : std_logic;
  signal iumsut : real;
  signal gkarhax : std_logic;
  signal xykow : real;
  signal ywkuuf : std_logic;
  signal nt : std_logic;
  signal sskvccbv : real;
begin
  upygnhmfou : entity work.flkif
    port map (eytuxkuni => sskvccbv, ddunt => nt, imhvlx => ywkuuf);
  ok : entity work.flkif
    port map (eytuxkuni => xykow, ddunt => gkarhax, imhvlx => nt);
  smjo : entity work.flkif
    port map (eytuxkuni => iumsut, ddunt => nt, imhvlx => ujgvopoe);
  
  -- Single-driven assignments
  t <= (others => 0 ns);
  peiev <= 16#1_7_8_6_6#;
  
  -- Multi-driven assignments
  nt <= 'X';
  nt <= 'U';
end zhnyiwbqkf;



-- Seed after: 4622005816343998957,13694093582652240945
