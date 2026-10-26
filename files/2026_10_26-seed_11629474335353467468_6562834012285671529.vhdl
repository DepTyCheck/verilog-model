-- Seed: 11629474335353467468,6562834012285671529

entity xspzm is
  port (bbpazsz : buffer real; pzczhlff : inout time; ckvcohpypz : inout time; mzjfxopu : out real);
end xspzm;

architecture rehqpl of xspzm is
  
begin
  -- Single-driven assignments
  mzjfxopu <= 240.2;
end rehqpl;

entity xow is
  port (mlfbd : inout integer);
end xow;

architecture i of xow is
  signal fsuzmzsn : real;
  signal ffnrfvkmuw : time;
  signal ovcofkgj : time;
  signal pmtuh : real;
begin
  yr : entity work.xspzm
    port map (bbpazsz => pmtuh, pzczhlff => ovcofkgj, ckvcohpypz => ffnrfvkmuw, mzjfxopu => fsuzmzsn);
end i;

library ieee;
use ieee.std_logic_1164.all;

entity bxegs is
  port (bhyxfto : buffer real_vector(0 downto 0); zghi : buffer integer; fiqgiywb : buffer std_logic; s : inout bit);
end bxegs;

architecture ityolkbi of bxegs is
  signal tlnv : integer;
  signal uvtz : real;
  signal ct : time;
  signal hwk : time;
  signal jbii : real;
  signal mhjsiknat : real;
  signal psgy : time;
  signal e : time;
  signal tqgyj : real;
begin
  ujoquzcom : entity work.xspzm
    port map (bbpazsz => tqgyj, pzczhlff => e, ckvcohpypz => psgy, mzjfxopu => mhjsiknat);
  jtjq : entity work.xspzm
    port map (bbpazsz => jbii, pzczhlff => hwk, ckvcohpypz => ct, mzjfxopu => uvtz);
  m : entity work.xow
    port map (mlfbd => tlnv);
  
  -- Single-driven assignments
  zghi <= zghi;
end ityolkbi;



-- Seed after: 8591463871670905565,6562834012285671529
