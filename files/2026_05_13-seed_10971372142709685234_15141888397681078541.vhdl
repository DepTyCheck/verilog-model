-- Seed: 10971372142709685234,15141888397681078541

library ieee;
use ieee.std_logic_1164.all;

entity qlor is
  port (pxerstn : out std_logic; fzztvtqnhj : in time);
end qlor;



architecture vgdfecfl of qlor is
  
begin
  
end vgdfecfl;

library ieee;
use ieee.std_logic_1164.all;

entity sxo is
  port (tabxold : inout std_logic; r : linkage boolean; yorakp : buffer time; nllpr : in time);
end sxo;



architecture jva of sxo is
  
begin
  uh : entity work.qlor
    port map (pxerstn => tabxold, fzztvtqnhj => yorakp);
  bmqqlfilz : entity work.qlor
    port map (pxerstn => tabxold, fzztvtqnhj => nllpr);
end jva;

library ieee;
use ieee.std_logic_1164.all;

entity anpa is
  port (dzsdmb : out std_logic);
end anpa;

library ieee;
use ieee.std_logic_1164.all;

architecture mhxuea of anpa is
  signal cufe : time;
  signal fpkuphsbnr : time;
  signal ygfimxudy : time;
  signal fmpnpzg : boolean;
  signal jf : std_logic;
begin
  wv : entity work.sxo
    port map (tabxold => jf, r => fmpnpzg, yorakp => ygfimxudy, nllpr => fpkuphsbnr);
  tqubset : entity work.qlor
    port map (pxerstn => dzsdmb, fzztvtqnhj => ygfimxudy);
  cw : entity work.qlor
    port map (pxerstn => dzsdmb, fzztvtqnhj => cufe);
end mhxuea;



-- Seed after: 11903567569685105284,15141888397681078541
