-- Seed: 15110000316132391711,5306691039457971049

entity wzoizthz is
  port (brgchuu : linkage integer; d : inout time);
end wzoizthz;

architecture rtbfczajrg of wzoizthz is
  
begin
  -- Single-driven assignments
  d <= d;
end rtbfczajrg;

entity dxnji is
  port (lwqj : in character; q : linkage integer);
end dxnji;

architecture trvk of dxnji is
  signal xzuytypnv : time;
  signal l : integer;
  signal uing : time;
  signal fmacpzui : time;
  signal xkwuho : integer;
  signal gvmbpv : time;
  signal vn : integer;
begin
  it : entity work.wzoizthz
    port map (brgchuu => vn, d => gvmbpv);
  fe : entity work.wzoizthz
    port map (brgchuu => xkwuho, d => fmacpzui);
  lrswbz : entity work.wzoizthz
    port map (brgchuu => q, d => uing);
  r : entity work.wzoizthz
    port map (brgchuu => l, d => xzuytypnv);
end trvk;

library ieee;
use ieee.std_logic_1164.all;

entity ovylhdain is
  port (w : out std_logic_vector(1 to 1); wddmrf : out integer; tmhzxivtg : buffer time);
end ovylhdain;

architecture n of ovylhdain is
  signal p : time;
  signal hhyes : integer;
  signal nrewtrb : time;
  signal yjdgp : integer;
  signal mqdxfgnw : time;
  signal vlfnontgtk : integer;
begin
  ohbuu : entity work.wzoizthz
    port map (brgchuu => vlfnontgtk, d => mqdxfgnw);
  teytddte : entity work.wzoizthz
    port map (brgchuu => yjdgp, d => nrewtrb);
  galrwit : entity work.wzoizthz
    port map (brgchuu => hhyes, d => p);
  hwblwposdh : entity work.wzoizthz
    port map (brgchuu => wddmrf, d => tmhzxivtg);
  
  -- Multi-driven assignments
  w <= (others => 'U');
  w <= (others => 'L');
end n;



-- Seed after: 15785213236731353170,5306691039457971049
