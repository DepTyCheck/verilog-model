-- Seed: 13995236512138393201,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity bp is
  port (fxba : in real; otmxxrmdac : linkage bit_vector(3 downto 2); nkdnmahawa : linkage std_logic);
end bp;

architecture zp of bp is
  
begin
  
end zp;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (jaahztwdum : inout std_logic);
end f;

architecture swvkp of f is
  signal zvzpaephca : bit_vector(3 downto 2);
  signal jxsb : real;
begin
  xxzlex : entity work.bp
    port map (fxba => jxsb, otmxxrmdac => zvzpaephca, nkdnmahawa => jaahztwdum);
  
  -- Single-driven assignments
  jxsb <= 8#50266.075#;
  
  -- Multi-driven assignments
  jaahztwdum <= jaahztwdum;
  jaahztwdum <= jaahztwdum;
end swvkp;

library ieee;
use ieee.std_logic_1164.all;

entity tfnht is
  port (ovsxgy : linkage std_logic_vector(4 to 1); vjqg : in std_logic_vector(2 downto 1));
end tfnht;

library ieee;
use ieee.std_logic_1164.all;

architecture zvolcoq of tfnht is
  signal w : bit_vector(3 downto 2);
  signal gckfkahza : std_logic;
  signal vzisbaod : bit_vector(3 downto 2);
  signal jjwlnkcnkx : std_logic;
  signal ctzdggncie : bit_vector(3 downto 2);
  signal owoqt : real;
begin
  bodpfnwfr : entity work.bp
    port map (fxba => owoqt, otmxxrmdac => ctzdggncie, nkdnmahawa => jjwlnkcnkx);
  rqptr : entity work.bp
    port map (fxba => owoqt, otmxxrmdac => vzisbaod, nkdnmahawa => gckfkahza);
  rlrgw : entity work.bp
    port map (fxba => owoqt, otmxxrmdac => w, nkdnmahawa => jjwlnkcnkx);
  dxoo : entity work.f
    port map (jaahztwdum => jjwlnkcnkx);
  
  -- Single-driven assignments
  owoqt <= 1_1_1_0_3.2_3_1;
  
  -- Multi-driven assignments
  jjwlnkcnkx <= 'Z';
  jjwlnkcnkx <= jjwlnkcnkx;
end zvolcoq;



-- Seed after: 12786871227397774623,13613332369802491303
