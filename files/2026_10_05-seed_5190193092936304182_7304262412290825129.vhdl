-- Seed: 5190193092936304182,7304262412290825129

entity jkhgk is
  port (qdq : buffer boolean; fntbcavtl : inout time);
end jkhgk;

architecture cwaac of jkhgk is
  
begin
  -- Single-driven assignments
  qdq <= FALSE;
  fntbcavtl <= fntbcavtl;
end cwaac;

entity oyvlhyuds is
  port (hmhacwabn : out boolean_vector(1 to 2); cjknsins : buffer boolean; gyhtbeb : buffer real);
end oyvlhyuds;

architecture ongrsfeg of oyvlhyuds is
  signal davgdze : time;
  signal aqpc : time;
  signal jjgtq : boolean;
  signal cwquoqc : time;
  signal k : boolean;
begin
  j : entity work.jkhgk
    port map (qdq => k, fntbcavtl => cwquoqc);
  qmkcdyecl : entity work.jkhgk
    port map (qdq => jjgtq, fntbcavtl => aqpc);
  euj : entity work.jkhgk
    port map (qdq => cjknsins, fntbcavtl => davgdze);
  
  -- Single-driven assignments
  hmhacwabn <= hmhacwabn;
end ongrsfeg;

library ieee;
use ieee.std_logic_1164.all;

entity xnagzj is
  port (hxhjs : buffer bit; vit : inout boolean_vector(2 downto 3); rwk : linkage std_logic; fnlqxx : inout integer);
end xnagzj;

architecture xt of xnagzj is
  signal euzmpimvuv : time;
  signal wdkoiq : boolean;
  signal liz : real;
  signal fupycmkcow : boolean;
  signal p : boolean_vector(1 to 2);
  signal fxkuxzavq : real;
  signal dvfgwcwaw : boolean;
  signal koomwro : boolean_vector(1 to 2);
  signal etvue : real;
  signal rufinp : boolean;
  signal qaigceqtdk : boolean_vector(1 to 2);
begin
  tywyl : entity work.oyvlhyuds
    port map (hmhacwabn => qaigceqtdk, cjknsins => rufinp, gyhtbeb => etvue);
  mz : entity work.oyvlhyuds
    port map (hmhacwabn => koomwro, cjknsins => dvfgwcwaw, gyhtbeb => fxkuxzavq);
  zegf : entity work.oyvlhyuds
    port map (hmhacwabn => p, cjknsins => fupycmkcow, gyhtbeb => liz);
  sjj : entity work.jkhgk
    port map (qdq => wdkoiq, fntbcavtl => euzmpimvuv);
  
  -- Single-driven assignments
  fnlqxx <= fnlqxx;
  hxhjs <= hxhjs;
  vit <= (others => TRUE);
end xt;



-- Seed after: 3842485156805718052,7304262412290825129
