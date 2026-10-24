-- Seed: 13090575272159239512,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity gqtgaunjfw is
  port (uuqg : inout real_vector(3 to 2); hcvttn : inout std_logic_vector(0 to 4); mn : buffer std_logic_vector(2 to 1));
end gqtgaunjfw;

architecture qwzcd of gqtgaunjfw is
  
begin
  -- Single-driven assignments
  uuqg <= uuqg;
  
  -- Multi-driven assignments
  mn <= (others => '0');
end qwzcd;

entity lbwyawoay is
  port (kuv : buffer severity_level; lwjns : out integer; umoezdwy : inout real_vector(3 to 4); pmhvmkpzho : buffer time);
end lbwyawoay;

library ieee;
use ieee.std_logic_1164.all;

architecture uvjqdqqznb of lbwyawoay is
  signal txc : real_vector(3 to 2);
  signal vja : real_vector(3 to 2);
  signal cwkfaei : std_logic_vector(2 to 1);
  signal s : real_vector(3 to 2);
  signal cmlj : std_logic_vector(2 to 1);
  signal ocfbfj : std_logic_vector(0 to 4);
  signal ywj : real_vector(3 to 2);
begin
  oza : entity work.gqtgaunjfw
    port map (uuqg => ywj, hcvttn => ocfbfj, mn => cmlj);
  peiggpiwnt : entity work.gqtgaunjfw
    port map (uuqg => s, hcvttn => ocfbfj, mn => cwkfaei);
  f : entity work.gqtgaunjfw
    port map (uuqg => vja, hcvttn => ocfbfj, mn => cmlj);
  aesj : entity work.gqtgaunjfw
    port map (uuqg => txc, hcvttn => ocfbfj, mn => cmlj);
  
  -- Single-driven assignments
  pmhvmkpzho <= 2313.4_4_0_3 us;
  lwjns <= 8#50#;
  umoezdwy <= (2#0101.11011#, 16#8.5#);
end uvjqdqqznb;



-- Seed after: 12893385273474127971,10975587770517081373
