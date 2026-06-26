-- Seed: 87995906408828334,12011142928354116943

entity gih is
  port (lyyp : buffer real_vector(4 downto 1); x : buffer real_vector(3 to 2));
end gih;

architecture unjvyu of gih is
  
begin
  -- Single-driven assignments
  x <= (others => 0.0);
  lyyp <= (24.2, 01.4_2_4_0, 16#A_7_4_C.C_E#, 2.02012);
end unjvyu;

library ieee;
use ieee.std_logic_1164.all;

entity mz is
  port (hrmip : in integer; xgqv : in std_logic_vector(1 to 3));
end mz;

architecture fdrca of mz is
  signal vzyadcnff : real_vector(3 to 2);
  signal vy : real_vector(4 downto 1);
  signal bykycynbb : real_vector(3 to 2);
  signal jszcoef : real_vector(4 downto 1);
begin
  tc : entity work.gih
    port map (lyyp => jszcoef, x => bykycynbb);
  mkcnw : entity work.gih
    port map (lyyp => vy, x => vzyadcnff);
end fdrca;



-- Seed after: 6165545651648598883,12011142928354116943
