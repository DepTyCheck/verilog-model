-- Seed: 2951679048822027292,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity wj is
  port (lapmnpyvyp : buffer std_logic_vector(2 to 1));
end wj;

architecture xfxucawtmi of wj is
  
begin
  
end xfxucawtmi;

entity ruccrfuz is
  port (rcxpxu : out real; bkhzwn : buffer boolean; xtlzecwfcy : buffer real);
end ruccrfuz;

library ieee;
use ieee.std_logic_1164.all;

architecture onsgb of ruccrfuz is
  signal hlgl : std_logic_vector(2 to 1);
  signal r : std_logic_vector(2 to 1);
begin
  izud : entity work.wj
    port map (lapmnpyvyp => r);
  mwbgnuc : entity work.wj
    port map (lapmnpyvyp => hlgl);
  maqqwdball : entity work.wj
    port map (lapmnpyvyp => r);
  vgzqq : entity work.wj
    port map (lapmnpyvyp => r);
  
  -- Single-driven assignments
  xtlzecwfcy <= rcxpxu;
  rcxpxu <= xtlzecwfcy;
  bkhzwn <= TRUE;
end onsgb;



-- Seed after: 15282426521301273774,9099465110516446487
