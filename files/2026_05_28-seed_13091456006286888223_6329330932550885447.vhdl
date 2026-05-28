-- Seed: 13091456006286888223,6329330932550885447



entity mk is
  port (nqey : linkage real);
end mk;



architecture loizmo of mk is
  
begin
  
end loizmo;



entity lg is
  port (ulhexj : buffer time; tqahyj : out integer);
end lg;



architecture lellmtoea of lg is
  
begin
  
end lellmtoea;

library ieee;
use ieee.std_logic_1164.all;

entity pkzrka is
  port ( de : buffer std_logic_vector(2 downto 0)
  ; vlycl : inout boolean_vector(4 downto 2)
  ; zpdrscvztk : buffer real_vector(1 to 3)
  ; jkijephlso : buffer std_logic
  );
end pkzrka;



architecture cimcxohuam of pkzrka is
  signal bc : real;
begin
  kodaavzatv : entity work.mk
    port map (nqey => bc);
end cimcxohuam;



entity knras is
  port (oj : out time; tqwhwyywnv : linkage real; q : linkage boolean_vector(2 to 3));
end knras;

library ieee;
use ieee.std_logic_1164.all;

architecture c of knras is
  signal u : integer;
  signal klppliz : std_logic;
  signal rktk : real_vector(1 to 3);
  signal w : boolean_vector(4 downto 2);
  signal p : std_logic_vector(2 downto 0);
begin
  fj : entity work.mk
    port map (nqey => tqwhwyywnv);
  ubljpieh : entity work.pkzrka
    port map (de => p, vlycl => w, zpdrscvztk => rktk, jkijephlso => klppliz);
  bhhxehtmi : entity work.lg
    port map (ulhexj => oj, tqahyj => u);
end c;



-- Seed after: 8718493585341375200,6329330932550885447
