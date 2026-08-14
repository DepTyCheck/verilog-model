-- Seed: 10417435842110234276,8437298063418820479

entity sojcyo is
  port (gertjyn : in integer);
end sojcyo;

architecture uozjyz of sojcyo is
  
begin
  
end uozjyz;

entity rjdynqrvsi is
  port (qygnimdx : linkage real; ytevllpvuv : linkage string(1 downto 4));
end rjdynqrvsi;

architecture bbswuvx of rjdynqrvsi is
  signal ecovtygmp : integer;
begin
  rt : entity work.sojcyo
    port map (gertjyn => ecovtygmp);
  
  -- Single-driven assignments
  ecovtygmp <= 0_2_1;
end bbswuvx;

library ieee;
use ieee.std_logic_1164.all;

entity ul is
  port (gtovzemzep : out std_logic_vector(4 to 0); koqaruvh : buffer std_logic_vector(4 downto 0));
end ul;

architecture me of ul is
  signal gywv : string(1 downto 4);
  signal kvecu : real;
begin
  aecbwxekvm : entity work.rjdynqrvsi
    port map (qygnimdx => kvecu, ytevllpvuv => gywv);
  
  -- Multi-driven assignments
  koqaruvh <= "1W1LH";
  gtovzemzep <= (others => '0');
end me;



-- Seed after: 14588545915536683381,8437298063418820479
