-- Seed: 7980394232122333065,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity tfrsxbg is
  port (gcva : buffer bit_vector(2 to 0); xovn : in integer; ffuis : in std_logic_vector(4 to 0));
end tfrsxbg;

architecture gul of tfrsxbg is
  
begin
  
end gul;

entity sc is
  port (k : inout time);
end sc;

library ieee;
use ieee.std_logic_1164.all;

architecture jxftcm of sc is
  signal gosfa : bit_vector(2 to 0);
  signal utafsconi : std_logic_vector(4 to 0);
  signal lwhxckgdrx : integer;
  signal ncwqgojl : bit_vector(2 to 0);
begin
  a : entity work.tfrsxbg
    port map (gcva => ncwqgojl, xovn => lwhxckgdrx, ffuis => utafsconi);
  jsbuvxu : entity work.tfrsxbg
    port map (gcva => gosfa, xovn => lwhxckgdrx, ffuis => utafsconi);
  
  -- Multi-driven assignments
  utafsconi <= "";
  utafsconi <= utafsconi;
  utafsconi <= utafsconi;
end jxftcm;

entity daztgpojlp is
  port (qrrg : in integer; eqeovs : in real);
end daztgpojlp;

library ieee;
use ieee.std_logic_1164.all;

architecture beqyn of daztgpojlp is
  signal jndru : integer;
  signal mlleptl : bit_vector(2 to 0);
  signal mkpynthxc : std_logic_vector(4 to 0);
  signal q : integer;
  signal hqafpelu : bit_vector(2 to 0);
begin
  bmumpmroa : entity work.tfrsxbg
    port map (gcva => hqafpelu, xovn => q, ffuis => mkpynthxc);
  qp : entity work.tfrsxbg
    port map (gcva => mlleptl, xovn => jndru, ffuis => mkpynthxc);
  
  -- Single-driven assignments
  q <= jndru;
  
  -- Multi-driven assignments
  mkpynthxc <= mkpynthxc;
  mkpynthxc <= (others => '0');
end beqyn;



-- Seed after: 13803759629200581328,12143220691580258643
