-- Seed: 16446966195284445744,16461708287571398341

entity shystc is
  port (rjy : buffer real; qpvdilnezd : inout real; vnluluna : buffer time);
end shystc;

architecture vur of shystc is
  
begin
  -- Single-driven assignments
  vnluluna <= 3_1_1_2_0 us;
end vur;

entity bkf is
  port (uady : in time; ilheysj : out real; lgvyhav : linkage string(5 to 4));
end bkf;

architecture oecqm of bkf is
  signal e : time;
  signal vj : real;
  signal iey : time;
  signal lolli : real;
  signal soizry : real;
begin
  opibit : entity work.shystc
    port map (rjy => soizry, qpvdilnezd => lolli, vnluluna => iey);
  sjku : entity work.shystc
    port map (rjy => vj, qpvdilnezd => ilheysj, vnluluna => e);
end oecqm;

library ieee;
use ieee.std_logic_1164.all;

entity gtswsmeu is
  port (hxpv : in time; f : buffer time; jpak : buffer std_logic);
end gtswsmeu;

architecture r of gtswsmeu is
  signal a : time;
  signal vdhllmseg : real;
  signal lqqyv : real;
  signal b : string(5 to 4);
  signal k : real;
begin
  sskrejiav : entity work.bkf
    port map (uady => f, ilheysj => k, lgvyhav => b);
  rrf : entity work.shystc
    port map (rjy => lqqyv, qpvdilnezd => vdhllmseg, vnluluna => a);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  jpak <= jpak;
  jpak <= jpak;
end r;



-- Seed after: 2614778460316450144,16461708287571398341
