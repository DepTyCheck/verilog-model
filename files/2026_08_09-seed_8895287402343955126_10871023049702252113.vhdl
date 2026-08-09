-- Seed: 8895287402343955126,10871023049702252113

entity qmednart is
  port (vag : inout time; beedh : buffer real; qyng : in real);
end qmednart;

architecture s of qmednart is
  
begin
  
end s;

library ieee;
use ieee.std_logic_1164.all;

entity jbrzevyims is
  port (lwb : in severity_level; yc : inout std_logic; eidkijehw : buffer boolean);
end jbrzevyims;

architecture wn of jbrzevyims is
  signal bavnu : real;
  signal nicwx : real;
  signal k : time;
  signal cmluzwtes : time;
  signal barhfxuq : real;
  signal zl : real;
  signal xldaeas : time;
begin
  y : entity work.qmednart
    port map (vag => xldaeas, beedh => zl, qyng => barhfxuq);
  ie : entity work.qmednart
    port map (vag => cmluzwtes, beedh => barhfxuq, qyng => barhfxuq);
  enxnqztab : entity work.qmednart
    port map (vag => k, beedh => nicwx, qyng => bavnu);
  
  -- Single-driven assignments
  eidkijehw <= FALSE;
  bavnu <= 8#6277.4_1_4_5_2#;
  
  -- Multi-driven assignments
  yc <= yc;
  yc <= '1';
  yc <= 'Z';
  yc <= 'L';
end wn;

library ieee;
use ieee.std_logic_1164.all;

entity tmqd is
  port (gydxrpngpe : in time; jrnrjjis : out std_logic; yw : out std_logic_vector(1 to 3); nsbhdj : inout std_logic);
end tmqd;

architecture gffgnk of tmqd is
  signal mp : boolean;
  signal yhbv : severity_level;
  signal wnhghadf : real;
  signal ji : time;
begin
  shudlayp : entity work.qmednart
    port map (vag => ji, beedh => wnhghadf, qyng => wnhghadf);
  cuyyam : entity work.jbrzevyims
    port map (lwb => yhbv, yc => nsbhdj, eidkijehw => mp);
  
  -- Single-driven assignments
  yhbv <= FAILURE;
  
  -- Multi-driven assignments
  yw <= yw;
  nsbhdj <= 'X';
  jrnrjjis <= nsbhdj;
end gffgnk;



-- Seed after: 11058679215508051560,10871023049702252113
