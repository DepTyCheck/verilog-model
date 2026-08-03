-- Seed: 6881410969488261784,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity ouvth is
  port ( pywnstkn : buffer integer_vector(4 downto 4)
  ; hgnhvjexk : out real_vector(0 to 2)
  ; iqah : out time_vector(4 to 3)
  ; skv : linkage std_logic_vector(4 downto 3)
  );
end ouvth;

architecture fu of ouvth is
  
begin
  -- Single-driven assignments
  iqah <= (others => 0 ns);
  hgnhvjexk <= (1.1_1_1_1_2, 3_4_2_0.2_2, 01.2);
  pywnstkn <= pywnstkn;
end fu;

entity maul is
  port (mnbfsast : inout real; rgkyhr : inout integer; lkyptl : linkage bit_vector(0 downto 1));
end maul;

library ieee;
use ieee.std_logic_1164.all;

architecture btx of maul is
  signal savisxij : std_logic_vector(4 downto 3);
  signal j : time_vector(4 to 3);
  signal gc : real_vector(0 to 2);
  signal hc : integer_vector(4 downto 4);
begin
  udioskxlu : entity work.ouvth
    port map (pywnstkn => hc, hgnhvjexk => gc, iqah => j, skv => savisxij);
  
  -- Multi-driven assignments
  savisxij <= savisxij;
end btx;



-- Seed after: 18315543798944447830,12359743974512393525
