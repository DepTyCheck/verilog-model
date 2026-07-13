-- Seed: 6962430420370623538,3566912872917928779

use std.reflection.all;

entity gajbsrlyt is
  port (f : inout enumeration_value_mirror; fpwnyebx : inout floating_value_mirror; ewblwexcc : out boolean);
end gajbsrlyt;

architecture uoupuip of gajbsrlyt is
  
begin
  -- Single-driven assignments
  ewblwexcc <= ewblwexcc;
end uoupuip;

use std.reflection.all;

entity ylqqoel is
  port (zmlew : inout array_value_mirror; xfvctv : in real);
end ylqqoel;

use std.reflection.all;

architecture dsaukxc of ylqqoel is
  signal bllu : boolean;
  shared variable qcwhj : floating_value_mirror;
  shared variable ddxaak : enumeration_value_mirror;
  signal yvmhrps : boolean;
  shared variable ikvf : floating_value_mirror;
  shared variable ewqslpne : enumeration_value_mirror;
  signal nmxkpa : boolean;
  shared variable ivqzxot : floating_value_mirror;
  shared variable beset : enumeration_value_mirror;
  signal ohvjolfpw : boolean;
  shared variable o : floating_value_mirror;
  shared variable bzkbeha : enumeration_value_mirror;
begin
  gucaaapb : entity work.gajbsrlyt
    port map (f => bzkbeha, fpwnyebx => o, ewblwexcc => ohvjolfpw);
  hlercwojza : entity work.gajbsrlyt
    port map (f => beset, fpwnyebx => ivqzxot, ewblwexcc => nmxkpa);
  kw : entity work.gajbsrlyt
    port map (f => ewqslpne, fpwnyebx => ikvf, ewblwexcc => yvmhrps);
  q : entity work.gajbsrlyt
    port map (f => ddxaak, fpwnyebx => qcwhj, ewblwexcc => bllu);
end dsaukxc;

entity u is
  port (kitmmpla : out real);
end u;

use std.reflection.all;

architecture yodkgepu of u is
  signal vsuvuwwc : boolean;
  shared variable qdl : floating_value_mirror;
  shared variable ihignbuqh : enumeration_value_mirror;
  signal vm : real;
  shared variable lk : array_value_mirror;
begin
  kwdqlg : entity work.ylqqoel
    port map (zmlew => lk, xfvctv => vm);
  znhlqukizg : entity work.gajbsrlyt
    port map (f => ihignbuqh, fpwnyebx => qdl, ewblwexcc => vsuvuwwc);
  
  -- Single-driven assignments
  kitmmpla <= kitmmpla;
  vm <= kitmmpla;
end yodkgepu;



-- Seed after: 5324800028883584512,3566912872917928779
