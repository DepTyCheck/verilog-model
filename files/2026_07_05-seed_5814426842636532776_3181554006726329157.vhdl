-- Seed: 5814426842636532776,3181554006726329157

use std.reflection.all;

entity ea is
  port (ld : inout array_subtype_mirror);
end ea;

architecture mvd of ea is
  
begin
  
end mvd;

use std.reflection.all;

entity nmllvgp is
  port (jg : inout floating_subtype_mirror; icanavdfo : linkage integer; us : inout real; xtnpmthz : inout integer);
end nmllvgp;

use std.reflection.all;

architecture u of nmllvgp is
  shared variable ouztlxkiw : array_subtype_mirror;
  shared variable iqyjeyigiq : array_subtype_mirror;
begin
  spdgbf : entity work.ea
    port map (ld => iqyjeyigiq);
  alrgzpcfw : entity work.ea
    port map (ld => ouztlxkiw);
end u;

use std.reflection.all;

entity vzyxtltat is
  port (faeekrbh : inout value_mirror; dwhcpoh : inout value_mirror; zwpg : linkage boolean_vector(3 downto 0); agtvvbx : inout physical_value_mirror);
end vzyxtltat;

use std.reflection.all;

architecture cltom of vzyxtltat is
  shared variable mwnevvpffb : array_subtype_mirror;
  shared variable whdmv : array_subtype_mirror;
  shared variable zmuudvzda : array_subtype_mirror;
begin
  xjidslv : entity work.ea
    port map (ld => zmuudvzda);
  malnkx : entity work.ea
    port map (ld => whdmv);
  r : entity work.ea
    port map (ld => mwnevvpffb);
end cltom;

use std.reflection.all;

entity qtiof is
  port (sp : inout integer; ijfx : buffer character; giggdekdx : inout protected_subtype_mirror; lkqelurqis : linkage severity_level);
end qtiof;

use std.reflection.all;

architecture sjw of qtiof is
  signal jbijbfi : integer;
  signal ehea : real;
  signal yvi : integer;
  shared variable qq : floating_subtype_mirror;
  shared variable xamngm : array_subtype_mirror;
  shared variable ethldyw : physical_value_mirror;
  signal bzvampe : boolean_vector(3 downto 0);
  shared variable hsjut : value_mirror;
  shared variable ptefkul : value_mirror;
begin
  wywsr : entity work.vzyxtltat
    port map (faeekrbh => ptefkul, dwhcpoh => hsjut, zwpg => bzvampe, agtvvbx => ethldyw);
  ocmcv : entity work.ea
    port map (ld => xamngm);
  wn : entity work.nmllvgp
    port map (jg => qq, icanavdfo => yvi, us => ehea, xtnpmthz => jbijbfi);
  
  -- Single-driven assignments
  sp <= 1_1_3;
  ijfx <= ijfx;
end sjw;



-- Seed after: 7052710190037996276,3181554006726329157
