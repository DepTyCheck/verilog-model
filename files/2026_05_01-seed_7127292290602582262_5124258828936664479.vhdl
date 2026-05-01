-- Seed: 7127292290602582262,5124258828936664479



entity pncutc is
  port (vb : out integer; czb : out bit);
end pncutc;



architecture mxd of pncutc is
  
begin
  
end mxd;

library ieee;
use ieee.std_logic_1164.all;

entity pe is
  port (qusblog : buffer std_logic; wt : out severity_level);
end pe;



architecture igyf of pe is
  signal zmt : bit;
  signal pa : integer;
  signal vb : bit;
  signal tsdriwjz : integer;
  signal rketmapgy : bit;
  signal v : integer;
  signal teqwqlgq : bit;
  signal jfuzlw : integer;
begin
  lrn : entity work.pncutc
    port map (vb => jfuzlw, czb => teqwqlgq);
  ve : entity work.pncutc
    port map (vb => v, czb => rketmapgy);
  rsypa : entity work.pncutc
    port map (vb => tsdriwjz, czb => vb);
  hr : entity work.pncutc
    port map (vb => pa, czb => zmt);
end igyf;



entity oh is
  port (uqtoajpcp : in integer; atbrqnul : inout real);
end oh;

library ieee;
use ieee.std_logic_1164.all;

architecture nehp of oh is
  signal szudds : severity_level;
  signal giifwvhfn : std_logic;
  signal ulq : bit;
  signal lvtcn : integer;
  signal dsobcrjza : bit;
  signal jqufj : integer;
  signal tp : bit;
  signal qpoqjlg : integer;
begin
  usgde : entity work.pncutc
    port map (vb => qpoqjlg, czb => tp);
  lab : entity work.pncutc
    port map (vb => jqufj, czb => dsobcrjza);
  hbgj : entity work.pncutc
    port map (vb => lvtcn, czb => ulq);
  iegkhc : entity work.pe
    port map (qusblog => giifwvhfn, wt => szudds);
end nehp;



-- Seed after: 2478018796026762324,5124258828936664479
