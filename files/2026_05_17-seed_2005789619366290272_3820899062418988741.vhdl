-- Seed: 2005789619366290272,3820899062418988741



entity vetwc is
  port (qilzahb : out time; clozyd : in time);
end vetwc;



architecture elkl of vetwc is
  
begin
  
end elkl;

library ieee;
use ieee.std_logic_1164.all;

entity aa is
  port (yy : linkage std_logic; nynam : linkage std_logic; tzatfukq : linkage integer);
end aa;



architecture naeecac of aa is
  signal ecqxxntmy : time;
  signal khhdanyx : time;
  signal jsizpudc : time;
  signal vtx : time;
  signal lymrsavpo : time;
  signal abphlwm : time;
begin
  dzji : entity work.vetwc
    port map (qilzahb => abphlwm, clozyd => lymrsavpo);
  uhjlnonka : entity work.vetwc
    port map (qilzahb => lymrsavpo, clozyd => abphlwm);
  tjjpm : entity work.vetwc
    port map (qilzahb => vtx, clozyd => jsizpudc);
  qwufze : entity work.vetwc
    port map (qilzahb => khhdanyx, clozyd => ecqxxntmy);
end naeecac;



entity sxztqyqm is
  port (gljuijry : out real);
end sxztqyqm;

library ieee;
use ieee.std_logic_1164.all;

architecture s of sxztqyqm is
  signal nxvym : integer;
  signal zlarjby : std_logic;
  signal aec : time;
  signal gea : time;
begin
  nscpqhy : entity work.vetwc
    port map (qilzahb => gea, clozyd => aec);
  xtjtdufk : entity work.vetwc
    port map (qilzahb => aec, clozyd => gea);
  fxc : entity work.aa
    port map (yy => zlarjby, nynam => zlarjby, tzatfukq => nxvym);
end s;



entity egw is
  port (ptabhfp : in real; yhvonglp : linkage real);
end egw;



architecture jitbhmiiw of egw is
  signal wkgl : time;
  signal o : time;
  signal dpbzvqwewu : time;
  signal jisr : time;
  signal m : real;
begin
  lzcrrofpzo : entity work.sxztqyqm
    port map (gljuijry => m);
  swn : entity work.vetwc
    port map (qilzahb => jisr, clozyd => jisr);
  qqkbolpjkl : entity work.vetwc
    port map (qilzahb => dpbzvqwewu, clozyd => o);
  yaxko : entity work.vetwc
    port map (qilzahb => o, clozyd => wkgl);
end jitbhmiiw;



-- Seed after: 5508174218034541371,3820899062418988741
