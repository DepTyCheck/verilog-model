-- Seed: 15722916270139022063,13788294582928374589

entity byrzree is
  port (t : buffer bit_vector(4 to 2));
end byrzree;

architecture b of byrzree is
  
begin
  -- Single-driven assignments
  t <= t;
end b;

entity yyrclmmux is
  port (vbtfzmm : buffer integer; irjhrasnql : in time);
end yyrclmmux;

architecture dv of yyrclmmux is
  signal ofmhspzgj : bit_vector(4 to 2);
  signal xhpuoj : bit_vector(4 to 2);
  signal vhjdhnujty : bit_vector(4 to 2);
  signal acgypeyn : bit_vector(4 to 2);
begin
  agubpiytb : entity work.byrzree
    port map (t => acgypeyn);
  fgkoy : entity work.byrzree
    port map (t => vhjdhnujty);
  f : entity work.byrzree
    port map (t => xhpuoj);
  cwlcnabe : entity work.byrzree
    port map (t => ofmhspzgj);
  
  -- Single-driven assignments
  vbtfzmm <= 143;
end dv;

library ieee;
use ieee.std_logic_1164.all;

entity xyg is
  port (fv : out real; remrle : linkage std_logic; zqrnhry : inout std_logic);
end xyg;

architecture va of xyg is
  signal ksayu : time;
  signal znsywab : integer;
begin
  eecfycpg : entity work.yyrclmmux
    port map (vbtfzmm => znsywab, irjhrasnql => ksayu);
  
  -- Single-driven assignments
  fv <= 0_0_0_2.3_2_1_1;
  ksayu <= 2#1_1.01010# ps;
  
  -- Multi-driven assignments
  zqrnhry <= zqrnhry;
  zqrnhry <= 'L';
  zqrnhry <= 'U';
  zqrnhry <= zqrnhry;
end va;

entity jpo is
  port (bjy : inout integer_vector(0 downto 4); urcamd : out integer);
end jpo;

architecture f of jpo is
  
begin
  -- Single-driven assignments
  urcamd <= urcamd;
  bjy <= (others => 0);
end f;



-- Seed after: 16286919808655912054,13788294582928374589
