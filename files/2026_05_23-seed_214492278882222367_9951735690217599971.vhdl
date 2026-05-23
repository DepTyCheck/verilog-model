-- Seed: 214492278882222367,9951735690217599971



entity csbponuyj is
  port (bhnnl : linkage time; xisthour : out bit);
end csbponuyj;



architecture li of csbponuyj is
  
begin
  
end li;



entity mp is
  port (urdabwxxb : in time);
end mp;



architecture kaddlgyhd of mp is
  signal ogaq : bit;
  signal kutli : bit;
  signal inf : bit;
  signal hz : bit;
  signal zuqllq : time;
begin
  fflwfkp : entity work.csbponuyj
    port map (bhnnl => zuqllq, xisthour => hz);
  fgjo : entity work.csbponuyj
    port map (bhnnl => urdabwxxb, xisthour => inf);
  lmw : entity work.csbponuyj
    port map (bhnnl => urdabwxxb, xisthour => kutli);
  acsvhnwt : entity work.csbponuyj
    port map (bhnnl => urdabwxxb, xisthour => ogaq);
end kaddlgyhd;

library ieee;
use ieee.std_logic_1164.all;

entity dwiduaigme is
  port (pjwhifwqp : linkage integer; kjudeonch : buffer std_logic; hlgsec : inout real; igsbjn : inout boolean);
end dwiduaigme;



architecture xx of dwiduaigme is
  signal rjjuczfuf : bit;
  signal p : bit;
  signal lqjjwyu : time;
begin
  b : entity work.csbponuyj
    port map (bhnnl => lqjjwyu, xisthour => p);
  mwymr : entity work.mp
    port map (urdabwxxb => lqjjwyu);
  jm : entity work.mp
    port map (urdabwxxb => lqjjwyu);
  sfrsupbgza : entity work.csbponuyj
    port map (bhnnl => lqjjwyu, xisthour => rjjuczfuf);
end xx;



-- Seed after: 66095405791238612,9951735690217599971
