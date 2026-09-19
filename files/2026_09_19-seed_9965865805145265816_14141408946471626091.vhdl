-- Seed: 9965865805145265816,14141408946471626091

entity o is
  port (pwrogqib : in integer; rqwdnzkxs : inout real);
end o;

architecture qvvatmuoxf of o is
  
begin
  -- Single-driven assignments
  rqwdnzkxs <= 0.42430;
end qvvatmuoxf;

library ieee;
use ieee.std_logic_1164.all;

entity fz is
  port (macnwu : out std_logic_vector(1 downto 2); unrxtg : inout real_vector(3 to 2); stveqbdrn : in time; yb : inout real);
end fz;

architecture deb of fz is
  signal fi : integer;
begin
  aelc : entity work.o
    port map (pwrogqib => fi, rqwdnzkxs => yb);
  
  -- Multi-driven assignments
  macnwu <= (others => '0');
end deb;

library ieee;
use ieee.std_logic_1164.all;

entity cbkcatoi is
  port (nkt : out std_logic);
end cbkcatoi;

library ieee;
use ieee.std_logic_1164.all;

architecture mxwhso of cbkcatoi is
  signal hvcsra : real;
  signal wp : real;
  signal dh : integer;
  signal rq : real;
  signal mc : integer;
  signal n : real;
  signal wdtml : time;
  signal tkcxuay : real_vector(3 to 2);
  signal cisjf : std_logic_vector(1 downto 2);
begin
  mmvmyx : entity work.fz
    port map (macnwu => cisjf, unrxtg => tkcxuay, stveqbdrn => wdtml, yb => n);
  pb : entity work.o
    port map (pwrogqib => mc, rqwdnzkxs => rq);
  dlcwemn : entity work.o
    port map (pwrogqib => dh, rqwdnzkxs => wp);
  sudve : entity work.o
    port map (pwrogqib => mc, rqwdnzkxs => hvcsra);
  
  -- Single-driven assignments
  wdtml <= 1 hr;
  dh <= mc;
  mc <= mc;
  
  -- Multi-driven assignments
  nkt <= 'H';
  nkt <= '-';
end mxwhso;



-- Seed after: 14031294039734170604,14141408946471626091
