-- Seed: 16028614293075103115,7726014785203345639

use std.reflection.all;

entity oetlnmh is
  port (madygus : inout record_value_mirror);
end oetlnmh;

architecture zvpegrdt of oetlnmh is
  
begin
  
end zvpegrdt;

use std.reflection.all;

entity rdm is
  port (gyn : in time; fn : inout value_mirror);
end rdm;

use std.reflection.all;

architecture svplsf of rdm is
  shared variable jlxcwuzip : record_value_mirror;
begin
  ajzcyyzj : entity work.oetlnmh
    port map (madygus => jlxcwuzip);
end svplsf;

use std.reflection.all;

entity pxf is
  port (ikyk : inout physical_subtype_mirror);
end pxf;

use std.reflection.all;

architecture nyh of pxf is
  shared variable xe : record_value_mirror;
  shared variable q : value_mirror;
  signal cild : time;
  shared variable gqmocut : record_value_mirror;
  shared variable belmo : record_value_mirror;
begin
  o : entity work.oetlnmh
    port map (madygus => belmo);
  uswvmto : entity work.oetlnmh
    port map (madygus => gqmocut);
  t : entity work.rdm
    port map (gyn => cild, fn => q);
  bqde : entity work.oetlnmh
    port map (madygus => xe);
  
  -- Single-driven assignments
  cild <= 4 min;
end nyh;



-- Seed after: 84597040147543089,7726014785203345639
