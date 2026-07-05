-- Seed: 8881446701620691693,3181554006726329157

use std.reflection.all;

entity pkfiyivo is
  port (tx : inout enumeration_value_mirror; rdevxnj : linkage integer; i : out integer; tjihwycxf : out bit_vector(0 to 4));
end pkfiyivo;

architecture smde of pkfiyivo is
  
begin
  -- Single-driven assignments
  i <= 16#2#;
  tjihwycxf <= ('1', '1', '0', '1', '0');
end smde;

use std.reflection.all;

entity iz is
  port (yql : inout enumeration_subtype_mirror; dr : inout value_mirror; cdcbztalqg : inout physical_subtype_mirror; agpzw : inout record_value_mirror);
end iz;

use std.reflection.all;

architecture d of iz is
  signal jogkdauh : bit_vector(0 to 4);
  signal olj : integer;
  signal wuohq : integer;
  shared variable awtrrtftz : enumeration_value_mirror;
begin
  atv : entity work.pkfiyivo
    port map (tx => awtrrtftz, rdevxnj => wuohq, i => olj, tjihwycxf => jogkdauh);
end d;

entity ecahs is
  port (squlg : buffer bit);
end ecahs;

use std.reflection.all;

architecture bb of ecahs is
  signal tnxhczy : bit_vector(0 to 4);
  signal ivrwhimqza : integer;
  signal hznpyiamo : integer;
  shared variable mwhojw : enumeration_value_mirror;
begin
  unshqprzp : entity work.pkfiyivo
    port map (tx => mwhojw, rdevxnj => hznpyiamo, i => ivrwhimqza, tjihwycxf => tnxhczy);
  
  -- Single-driven assignments
  squlg <= '1';
end bb;



-- Seed after: 5778342553510810053,3181554006726329157
