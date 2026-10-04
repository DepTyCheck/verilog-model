-- Seed: 5976678068782193134,15795020531041709203

entity aduvyig is
  port (dunhddrtmz : in real; sbyhbt : inout string(5 to 4));
end aduvyig;

architecture vae of aduvyig is
  
begin
  -- Single-driven assignments
  sbyhbt <= (others => ' ');
end vae;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (foen : buffer real; hn : linkage std_logic);
end t;

architecture n of t is
  signal gzm : string(5 to 4);
  signal otnf : real;
  signal oqmgvoydbg : string(5 to 4);
  signal gvtet : string(5 to 4);
  signal kfamywbbaa : string(5 to 4);
  signal gq : real;
begin
  fqdgff : entity work.aduvyig
    port map (dunhddrtmz => gq, sbyhbt => kfamywbbaa);
  pxmg : entity work.aduvyig
    port map (dunhddrtmz => gq, sbyhbt => gvtet);
  eodokf : entity work.aduvyig
    port map (dunhddrtmz => foen, sbyhbt => oqmgvoydbg);
  uljugezekh : entity work.aduvyig
    port map (dunhddrtmz => otnf, sbyhbt => gzm);
  
  -- Single-driven assignments
  otnf <= foen;
  gq <= foen;
  foen <= gq;
end n;

entity c is
  port (vpz : linkage bit; tatzgkgb : out integer);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture czjm of c is
  signal gusak : std_logic;
  signal y : string(5 to 4);
  signal ut : real;
  signal beuz : string(5 to 4);
  signal qgw : real;
  signal vujay : string(5 to 4);
  signal ewehq : real;
begin
  ulrlusm : entity work.aduvyig
    port map (dunhddrtmz => ewehq, sbyhbt => vujay);
  cdzee : entity work.aduvyig
    port map (dunhddrtmz => qgw, sbyhbt => beuz);
  vhyjmu : entity work.aduvyig
    port map (dunhddrtmz => ut, sbyhbt => y);
  m : entity work.t
    port map (foen => qgw, hn => gusak);
  
  -- Single-driven assignments
  ut <= ewehq;
  ewehq <= 16#F.F3#;
  
  -- Multi-driven assignments
  gusak <= 'L';
  gusak <= gusak;
end czjm;



-- Seed after: 6871159212495702118,15795020531041709203
